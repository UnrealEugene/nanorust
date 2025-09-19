use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};
use chumsky::span::SimpleSpan;
use hashbrown::HashMap;

use crate::ir::{FuncInfo, IR, Node, RValue, Reference, Tree, Value};

pub type FuncOverride = Box<dyn Fn(&[RValue]) -> Result<RValue, String> + 'static>;

pub struct InterpretEnv<'src> {
    stack_trace: Vec<(SimpleSpan, &'src str)>,
    var_stack: Vec<(&'src str, RValue)>,
    func_overrides: HashMap<usize, FuncOverride>,
}

impl<'src> InterpretEnv<'src> {
    pub fn new() -> Self {
        Self {
            stack_trace: Vec::new(),
            var_stack: Vec::new(),
            func_overrides: HashMap::new(),
        }
    }
}

pub type InterpretResult = Result<RValue, Panic>;

#[derive(Debug, Clone)]
pub struct Panic {
    message: String,
    stack_trace: String,
}

impl Panic {
    pub fn new(message: impl AsRef<str>) -> Self {
        Self {
            message: message.as_ref().to_string(),
            stack_trace: "".into(),
        }
    }

    pub fn message(&self) -> &str {
        self.message.as_str()
    }

    pub fn stack_trace(&self) -> &str {
        self.stack_trace.as_str()
    }
}

type ControlFlow<T> = Result<T, UnwindReason>;

enum UnwindReason {
    Panic(String),
    Return(RValue),
    Break,
    Continue,
}

impl<'src> InterpretEnv<'src> {
    pub fn register_builtin(
        &mut self,
        func_table: &Vec<FuncInfo<'src>>,
        func_name: impl AsRef<str>,
        func_override: FuncOverride,
    ) {
        let func_index = func_table
            .iter()
            .position(|info| info.global && info.name.0 == func_name.as_ref())
            .unwrap_or_else(|| {
                panic!(
                    "attempt to override an undefined function {}",
                    func_name.as_ref()
                )
            });
        self.func_overrides.insert(func_index, func_override);
    }

    pub fn register_builtins(
        &mut self,
        func_table: &Vec<FuncInfo<'src>>,
        entries: Box<[(&'src str, FuncOverride)]>,
    ) {
        for (func_name, func_override) in entries {
            self.register_builtin(func_table, func_name, func_override);
        }
    }

    pub fn interpret(&mut self, ir: &IR<'src>) -> InterpretResult {
        let result = self
            .interpret_node(ir.root(), ir)
            .and_then(|val| val.to_rvalue(self));
        Ok(match result {
            Ok(value) => value,
            Err(UnwindReason::Return(value)) => value,
            Err(UnwindReason::Panic(message)) => return Err(Panic::new(message)),
            _ => unreachable!(),
        })
    }

    fn interpret_node(&mut self, node: &Node<'src>, ir: &IR<'src>) -> ControlFlow<Value> {
        Ok(match node.tree() {
            Tree::Constant { value } => value.clone(),
            Tree::Scope { nodes } => {
                let stack_len = self.var_stack.len();
                let x = nodes
                    .iter()
                    .map(|node| self.interpret_node(node, ir))
                    .collect::<ControlFlow<Vec<_>>>()?;
                self.var_stack.shrink_to(stack_len);
                x.last()
                    .cloned()
                    .unwrap_or_default()
                    .to_rvalue(&self)?
                    .into()
            }
            Tree::Call { callee, args } => {
                let value = self.interpret_node(callee, ir)?.to_rvalue(self)?;
                let func_index = match value {
                    RValue::Reference(reference) => match reference {
                        Reference::Function(index) => index,
                        Reference::Variable(index) => {
                            return Err(UnwindReason::Panic(format!(
                                "attempt to call a non-callable variable {}",
                                self.var_stack
                                    .get(self.var_stack.len() - index - 1)
                                    .unwrap()
                                    .0
                            )));
                        }
                    },
                    _ => {
                        return Err(UnwindReason::Panic(format!(
                            "attempt to call a non-callable value {:?}",
                            value
                        )));
                    }
                };
                let func_info = ir.function_table().get(func_index).unwrap();
                if args.len() != func_info.args.len() {
                    return Err(UnwindReason::Panic(format!(
                        "expected {} arguments when calling a function, actual {}",
                        func_info.args.len(),
                        args.len()
                    )));
                }
                let arg_vals = args
                    .iter()
                    .map(|node| {
                        self.interpret_node(node, ir)
                            .and_then(|v| v.to_rvalue(self))
                    })
                    .collect::<ControlFlow<Vec<_>>>()?;
                self.stack_trace.push((node.span(), func_info.name.0));
                let result_value = match self.func_overrides.get(&func_index) {
                    Some(f) => f(&arg_vals).map_err(UnwindReason::Panic)?,
                    None => {
                        let stack_len = self.var_stack.len();
                        func_info
                            .args
                            .iter()
                            .zip(arg_vals.into_iter())
                            .for_each(|(name, value)| self.var_stack.push((name.0, value)));
                        let result = self.interpret_node(func_info.body.as_ref().unwrap(), ir);
                        let result_value = match result {
                            Ok(value) => value.to_rvalue(self)?,
                            Err(UnwindReason::Return(value)) => value,
                            Err(UnwindReason::Panic(_)) => return result,
                            _ => unreachable!(),
                        };
                        self.var_stack.truncate(stack_len);
                        result_value
                    }
                };
                self.stack_trace.pop();
                result_value.into()
            }
            Tree::Assign { assignee, value } => {
                let assignee_value = self.interpret_node(assignee, ir)?;
                let value = self.interpret_node(value, ir)?.to_rvalue(self)?;
                let index = match assignee_value {
                    Value::LValue(reference) => match reference {
                        Reference::Variable(index) => index,
                        Reference::Function(index) => {
                            return Err(UnwindReason::Panic(format!(
                                "attempt to assign to a function {}",
                                ir.function_table().get(index).unwrap().name.0
                            )));
                        }
                    },
                    Value::RValue(val) => {
                        return Err(UnwindReason::Panic(format!(
                            "attempt to assign to a rvalue {:?}",
                            val
                        )));
                    }
                };
                let index = self.var_stack.len() - index - 1;
                self.var_stack.get_mut(index).unwrap().1 = value;
                Value::unit()
            }
            Tree::Let {
                variable,
                mutable: _,
                value,
            } => {
                let value = self.interpret_node(value, ir)?.to_rvalue(self)?;
                self.var_stack.push((variable.name.0, value));
                Value::unit()
            }
            Tree::If {
                condition,
                if_then,
                if_else,
            } => {
                let cond_value = match self.interpret_node(condition, ir)?.to_rvalue(self)? {
                    RValue::Boolean(val) => val,
                    val => {
                        return Err(UnwindReason::Panic(format!(
                            "exepcted boolean value as if expression condition, actual {:?}",
                            val
                        )));
                    }
                };

                self.interpret_node(if cond_value { if_then } else { if_else }, ir)?
                    .to_rvalue(self)?
                    .into()
            }
            Tree::While { condition, body } => loop {
                let cond_value = match self.interpret_node(condition, ir)?.to_rvalue(self)? {
                    RValue::Boolean(val) => val,
                    val => {
                        return Err(UnwindReason::Panic(format!(
                            "exepcted boolean value as while statement condition, actual {:?}",
                            val
                        )));
                    }
                };
                if !cond_value {
                    break Value::unit();
                }
                let body_result = self.interpret_node(body, ir);
                match body_result {
                    Ok(_) => {}
                    Err(UnwindReason::Panic(_)) | Err(UnwindReason::Return(_)) => {
                        return body_result;
                    }
                    Err(UnwindReason::Break) => break Value::unit(),
                    Err(UnwindReason::Continue) => continue,
                };
            },
            Tree::Return { value } => {
                return Err(UnwindReason::Return(
                    self.interpret_node(value, ir)?.to_rvalue(self)?,
                ));
            }
            Tree::Break => return Err(UnwindReason::Break),
            Tree::Continue => return Err(UnwindReason::Continue),
        })
    }
}

impl Value {
    fn to_rvalue<'src>(self, env: &InterpretEnv<'src>) -> ControlFlow<RValue> {
        match self {
            Value::RValue(rvalue) => Ok(rvalue),
            Value::LValue(reference) => match &reference {
                Reference::Function(_) => Ok(RValue::Reference(reference)),
                Reference::Variable(index) => env
                    .var_stack
                    .get(env.var_stack.len() - *index - 1)
                    .ok_or_else(|| {
                        UnwindReason::Panic(format!(
                            "illegal variable reference {:?}, stack length is {}",
                            &reference,
                            env.var_stack.len()
                        ))
                    })
                    .map(|(_, val)| val)
                    .cloned(),
            },
        }
    }
}
