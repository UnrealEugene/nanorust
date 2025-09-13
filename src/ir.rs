use core::borrow::Borrow;

use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use crate::{expr::Expr, span::Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
    Function(usize),
    Variable(usize),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum RValue {
    #[default]
    Unit,
    Number(i32),
    Reference(Reference),
}

impl Into<Value> for RValue {
    fn into(self) -> Value {
        Value::RValue(self)
    }
}

fn ordinal(n: usize) -> String {
    match n {
        1 => "1st".into(),
        2 => "2nd".into(),
        _ => format!("{}th", n),
    }
}

impl RValue {
    pub fn unwrap_number(self, pos: usize) -> Result<i32, InterpretError> {
        match self {
            RValue::Number(x) => Ok(x),
            _ => Err(InterpretError::new(format!(
                "{} argument is not a number",
                ordinal(pos)
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    RValue(RValue),
    LValue(Reference),
}

impl Default for Value {
    fn default() -> Self {
        Self::RValue(RValue::default())
    }
}

impl Value {
    fn unit() -> Self {
        Self::default()
    }

    fn to_rvalue<'src>(self, env: &InterpretEnv<'src>) -> InterpretResult {
        match self {
            Value::RValue(rvalue) => Ok(rvalue),
            Value::LValue(reference) => match &reference {
                Reference::Function(_) => Ok(RValue::Reference(reference)),
                Reference::Variable(index) => env
                    .var_stack
                    .get(env.var_stack.len() - *index - 1)
                    .ok_or_else(|| {
                        InterpretError::new(format!(
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

pub type Builtin = Box<dyn Fn(&[RValue]) -> InterpretResult + 'static>;

pub enum FuncBody<'src> {
    Builtin(Builtin),
    Node(Node<'src>),
    Unknown,
}

pub struct FuncInfo<'src> {
    name: &'src str,
    args: Vec<&'src str>,
    global: bool,
    body: FuncBody<'src>,
}

pub struct IR<'src> {
    func_table: Vec<FuncInfo<'src>>,
    root: Node<'src>,
}

enum IRSymbol<'src> {
    Variable,
    Function { index: usize, info: FuncInfo<'src> },
}

struct IRBuilder<'src> {
    func_table: Vec<Option<FuncInfo<'src>>>,
    symbol_stack: Vec<Vec<(&'src str, IRSymbol<'src>)>>,
    scope_stack: Vec<Vec<Node<'src>>>,
}

impl<'src> IRBuilder<'src> {
    fn new() -> Self {
        IRBuilder {
            func_table: Vec::new(),
            symbol_stack: Vec::new(),
            scope_stack: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.symbol_stack.push(Vec::new());
        self.scope_stack.push(Vec::new());
    }

    fn pop_scope(&mut self) -> Vec<Node<'src>> {
        self.symbol_stack
            .pop()
            .unwrap()
            .into_iter()
            .for_each(|(_, symbol)| match symbol {
                IRSymbol::Variable => {}
                IRSymbol::Function { index, info } => {
                    self.func_table.get_mut(index).unwrap().replace(info);
                }
            });

        self.scope_stack.pop().unwrap()
    }

    fn push_frame(&mut self, args: Vec<&'src str>) {
        self.symbol_stack.push(
            args.into_iter()
                .map(|name| (name, IRSymbol::Variable))
                .collect(),
        );
    }

    fn pop_frame(&mut self) {
        self.symbol_stack.pop();
    }

    fn push_symbol(&mut self, name: &'src str, symbol: IRSymbol<'src>) {
        self.symbol_stack.last_mut().unwrap().push((name, symbol));
    }

    fn pop_symbol(&mut self) -> IRSymbol<'src> {
        self.symbol_stack.last_mut().unwrap().pop().unwrap().1
    }

    fn push_node(&mut self, node: Node<'src>) {
        if node != Node::default() {
            self.scope_stack.last_mut().unwrap().push(node)
        }
    }

    fn resolve_symbol(&self, name: &'src str) -> Reference {
        let mut var_index = 0usize;
        for (symbol_name, symbol) in self.symbol_stack.iter().flatten().rev() {
            if *symbol_name == name {
                return match symbol {
                    IRSymbol::Variable => Reference::Variable(var_index),
                    IRSymbol::Function { index, .. } => Reference::Function(*index),
                };
            }
            if let IRSymbol::Variable = symbol {
                var_index += 1;
            }
        }
        panic!("undefined identifier {}", name)
    }
}

impl<'src> IR<'src> {
    pub fn root(&self) -> &Node<'src> {
        &self.root
    }

    pub fn from_ast(ast: &Spanned<Expr<'src>>) -> Self {
        let mut builder = IRBuilder::new();
        builder.push_scope();
        let node = Self::from_ast_impl(ast, &mut builder);
        let scope = builder.pop_scope();
        IR {
            func_table: builder.func_table.into_iter().map(Option::unwrap).collect(),
            root: Node::new(Tree::Scope {
                nodes: if scope.is_empty() { vec![node] } else { scope },
            }),
        }
    }

    fn from_ast_impl(expr: &Spanned<Expr<'src>>, builder: &mut IRBuilder<'src>) -> Node<'src> {
        match &expr.0 {
            Expr::Skip => Node::default(),
            Expr::Block(expr) => {
                builder.push_scope();
                let node = Self::from_ast_impl(expr.as_ref(), builder);
                let scope = builder.pop_scope();
                Node::new(Tree::Scope {
                    nodes: if scope.is_empty() { vec![node] } else { scope },
                })
            }
            Expr::Ignore(expr) => {
                let node = Self::from_ast_impl(expr.as_ref(), builder);
                Node::new(Tree::Ignore {
                    node: Box::new(node),
                })
            }
            Expr::Location { name, .. } => Node::new(Tree::Constant {
                value: Value::LValue(builder.resolve_symbol(name.0)),
            }),
            Expr::Constant(value) => Node::new(Tree::Constant {
                value: match value {
                    crate::value::Value::Number(x) => Value::RValue(RValue::Number(*x)),
                    _ => todo!(),
                },
            }),
            Expr::Unary(_op, _arg) => todo!(),
            Expr::Binary(op, left, right) => {
                let func_ref = builder.resolve_symbol(op.get_builtin_name());
                Node::new(Tree::Call {
                    callee: Box::new(Node::new(Tree::Constant {
                        value: Value::LValue(func_ref),
                    })),
                    args: [left, right]
                        .iter()
                        .map(|arg| Self::from_ast_impl(arg, builder))
                        .collect(),
                })
            }
            Expr::Assign(lhs, rhs) => Node::new(Tree::Assign {
                assignee: Box::new(Self::from_ast_impl(lhs.as_ref(), builder)),
                value: Box::new(Self::from_ast_impl(rhs.as_ref(), builder)),
            }),
            Expr::Seq(first, second) => {
                let first_node = Self::from_ast_impl(first.as_ref(), builder);
                builder.push_node(first_node);
                let second_node = Self::from_ast_impl(second.as_ref(), builder);
                match &second.as_ref().0 {
                    Expr::Seq(..) => {}
                    Expr::VarScope { .. } => {}
                    Expr::FunScope { .. } => {}
                    _ => {
                        builder.push_node(second_node);
                    }
                }
                Node::default()
            }
            Expr::VarScope {
                var,
                is_mut: _,
                val,
                cont,
            } => {
                let value_node = Self::from_ast_impl(val.as_ref(), builder);
                builder.push_node(Node::new(Tree::Let {
                    variable: var.name.0,
                    value: Box::new(value_node),
                }));
                builder.push_symbol(var.name.0, IRSymbol::Variable);
                let cont_node = Self::from_ast_impl(cont.as_ref(), builder);
                match &cont.as_ref().0 {
                    Expr::Seq(..) => {}
                    Expr::VarScope { .. } => {}
                    Expr::FunScope { .. } => {}
                    _ => {
                        builder.push_node(cont_node);
                    }
                }
                Node::default()
            }
            Expr::FunScope {
                name, func, cont, ..
            } => {
                let args: Vec<_> = func.args.iter().map(|a| a.0).collect();
                let func_symbol = IRSymbol::Function {
                    index: builder.func_table.len(),
                    info: FuncInfo {
                        name: name.0,
                        args: args.clone(),
                        global: builder.scope_stack.len() <= 1,
                        body: FuncBody::Unknown,
                    },
                };
                builder.push_symbol(name.0, func_symbol);
                builder.push_frame(args);

                let body_node = Self::from_ast_impl(&func.body, builder);
                
                builder.pop_frame();
                let mut func_symbol = builder.pop_symbol();
                if let IRSymbol::Function { ref mut info, .. } = func_symbol {
                    info.body = FuncBody::Node(body_node);
                } else {
                    unreachable!()
                }
                builder.push_symbol(name.0, func_symbol);

                builder.func_table.push(None);
                let cont_node = Self::from_ast_impl(cont.as_ref(), builder);
                match &cont.as_ref().0 {
                    Expr::Seq(..) => {}
                    Expr::VarScope { .. } => {}
                    Expr::FunScope { .. } => {}
                    _ => {
                        builder.push_node(cont_node);
                    }
                }
                Node::default()
            }
            Expr::Call(callee, args) => Node::new(Tree::Call {
                callee: Box::new(Self::from_ast_impl(callee.as_ref(), builder)),
                args: args
                    .iter()
                    .map(|arg| Self::from_ast_impl(arg, builder))
                    .collect(),
            }),
            _ => todo!(),
        }
    }

    pub fn register_builtin(
        &mut self,
        name: impl AsRef<str>,
        builtin: impl Fn(&[RValue]) -> InterpretResult + 'static,
    ) {
        self.func_table
            .iter_mut()
            .find(|info| info.name == name.as_ref() && info.global)
            .unwrap_or_else(|| panic!("attempt to redefine undefined function {}", name.as_ref()))
            .body = FuncBody::Builtin(Box::new(builtin));
    }
}

#[derive(Debug, PartialEq)]
pub struct Node<'src> {
    // span: SimpleSpan,
    tree: Tree<'src>,
}

impl<'src> Default for Node<'src> {
    fn default() -> Self {
        Self {
            tree: Tree::Scope { nodes: Vec::new() },
        }
    }
}

impl<'src> Node<'src> {
    fn new(tree: Tree<'src>) -> Self {
        Self { tree }
    }
}

#[derive(Debug, PartialEq)]
pub enum Tree<'src> {
    Constant {
        value: Value,
    },

    Ignore {
        node: Box<Node<'src>>,
    },

    Scope {
        nodes: Vec<Node<'src>>,
    },

    Call {
        callee: Box<Node<'src>>,
        args: Vec<Node<'src>>,
    },

    Assign {
        assignee: Box<Node<'src>>,
        value: Box<Node<'src>>,
    },

    Let {
        variable: &'src str,
        value: Box<Node<'src>>,
    },
}

pub struct InterpretEnv<'src> {
    var_stack: Vec<(&'src str, RValue)>,
}

impl<'src> InterpretEnv<'src> {
    pub fn new() -> Self {
        Self {
            var_stack: Vec::new(),
        }
    }
}

pub type InterpretResult = Result<RValue, InterpretError>;

#[derive(Debug, Clone)]
pub struct InterpretError {
    _message: String,
}

impl InterpretError {
    pub fn new(message: impl AsRef<str>) -> Self {
        Self {
            _message: message.as_ref().to_string(),
        }
    }
}

impl<'src> InterpretEnv<'src> {
    pub fn interpret(&mut self, ir: &IR<'src>) -> InterpretResult {
        self.interpret_node(&ir.root, ir)
            .and_then(|v| v.to_rvalue(&self))
    }

    fn interpret_node(
        &mut self,
        node: &Node<'src>,
        ir: &IR<'src>,
    ) -> Result<Value, InterpretError> {
        Ok(match &node.borrow().tree {
            Tree::Constant { value } => value.clone(),
            Tree::Ignore { node } => {
                let _ = self.interpret_node(node, ir)?;
                Value::unit()
            }
            Tree::Scope { nodes } => {
                let stack_len = self.var_stack.len();
                let x = nodes
                    .iter()
                    .map(|node| self.interpret_node(node, ir))
                    .collect::<Result<Vec<_>, InterpretError>>()?;
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
                            return Err(InterpretError::new(format!(
                                "attempt to call a non-callable variable {}",
                                self.var_stack
                                    .get(self.var_stack.len() - index - 1)
                                    .unwrap()
                                    .0
                            )))
                        }
                    },
                    _ => {
                        return Err(InterpretError::new(format!(
                            "attempt to call a non-callable value {:?}",
                            value
                        )))
                    }
                };
                let func_info = ir.func_table.get(func_index).unwrap();
                if args.len() != func_info.args.len() {
                    return Err(InterpretError::new(format!(
                        "expected {} arguments when calling a function, actual {}",
                        func_info.args.len(),
                        args.len()
                    )));
                }
                let arg_vals = args
                    .iter()
                    .map(|node| {
                        self.interpret_node(node, ir)
                            .and_then(|v| v.to_rvalue(&self))
                    })
                    .collect::<Result<Vec<_>, InterpretError>>()?;
                match &func_info.body {
                    FuncBody::Builtin(f) => f(&arg_vals)?.into(),
                    FuncBody::Node(node) => {
                        let stack_len = self.var_stack.len();
                        func_info
                            .args
                            .iter()
                            .zip(arg_vals.into_iter())
                            .for_each(|(name, value)| self.var_stack.push((*name, value)));
                        let result = self
                            .interpret_node(node, ir)
                            .and_then(|v| v.to_rvalue(&self))?;
                        self.var_stack.shrink_to(stack_len);
                        Value::RValue(result)
                    }
                    FuncBody::Unknown => unreachable!(),
                }
            }
            Tree::Assign { assignee, value } => {
                let assignee_val = self.interpret_node(assignee, ir)?;
                let index = match assignee_val {
                    Value::LValue(reference) => match reference {
                        Reference::Variable(index) => index,
                        Reference::Function(index) => {
                            return Err(InterpretError::new(format!(
                                "attempt to assign to a function {}",
                                ir.func_table.get(index).unwrap().name
                            )))
                        }
                    },
                    Value::RValue(val) => {
                        return Err(InterpretError::new(format!(
                            "attempt to assign to a rvalue {:?}",
                            val
                        )))
                    }
                };
                let value = self
                    .interpret_node(value, ir)
                    .and_then(|v| v.to_rvalue(&self))?;
                self.var_stack.get_mut(index).unwrap().1 = value;
                Value::unit()
            }
            Tree::Let { variable, value } => {
                let value = self
                    .interpret_node(value, ir)
                    .and_then(|v| v.to_rvalue(&self))?;
                self.var_stack.push((*variable, value));
                Value::unit()
            }
        })
    }
}
