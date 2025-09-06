use core::borrow::Borrow;

use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use hashbrown::HashMap;

use crate::{expr::Expr, span::Spanned};

#[derive(Debug, Clone, Default, PartialEq)]
pub enum RValue {
    #[default]
    Unit,
    Number(i32),
}

impl Into<Value> for RValue {
    fn into(self) -> Value {
        Value::RValue(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    RValue(RValue),
    LValue(String),
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
            Value::LValue(name) => env
                .var_stack
                .iter()
                .rev()
                .find(|(var, _)| *var == name)
                .map(|e| e.1.clone())
                .ok_or_else(|| InterpretError::new(format!("undefined variable {}", &name))),
        }
    }
}

pub struct Builtin<'src> {
    args: Vec<&'src str>,
    func: Box<dyn Fn(Vec<RValue>) -> InterpretResult>,
}

pub struct IR<'src> {
    builtins: HashMap<String, Builtin<'src>>,
    root: Node<'src>,
}

struct IRBuilder<'src> {
    scope_stack: Vec<Vec<Node<'src>>>,
}

impl<'src> IRBuilder<'src> {
    fn new() -> Self {
        IRBuilder {
            scope_stack: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    fn pop_scope(&mut self) -> Vec<Node<'src>> {
        self.scope_stack.pop().unwrap()
    }

    fn push_node(&mut self, node: Node<'src>) {
        if node != Node::default() {
            self.scope_stack.last_mut().unwrap().push(node)
        }
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
            builtins: HashMap::new(),
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
                value: Value::LValue(name.0.to_string()),
            }),
            Expr::Constant(value) => Node::new(Tree::Constant {
                value: match value {
                    crate::value::Value::Number(x) => Value::RValue(RValue::Number(*x)),
                    _ => todo!(),
                },
            }),
            Expr::Unary(_op, _arg) => todo!(),
            Expr::Binary(_op, _left, _right) => todo!(),
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
                let value = self.interpret_node(callee, ir)?;
                let Value::LValue(name) = value else {
                    return Err(InterpretError::new(format!(
                        "attempt to call on rvalue {:?}",
                        value
                    )));
                };
                let Builtin {
                    args: arg_names,
                    func,
                } = ir
                    .builtins
                    .get(&name)
                    .ok_or_else(|| InterpretError::new(format!("undefined function {}", &name)))?;
                if args.len() != arg_names.len() {
                    return Err(InterpretError::new(format!(
                        "expected {} arguments when calling a function, actual {}",
                        arg_names.len(),
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
                func(arg_vals)?.into()
            }
            Tree::Assign { assignee, value } => {
                let assignee_val = self.interpret_node(assignee, ir)?;
                let Value::LValue(name) = assignee_val else {
                    return Err(InterpretError::new(format!(
                        "attempt to assign to a rvalue {:?}",
                        value
                    )));
                };
                let value = self
                    .interpret_node(value, ir)
                    .and_then(|v| v.to_rvalue(&self))?;
                self.var_stack
                    .iter_mut()
                    .rev()
                    .find(|(var, _)| *var == name)
                    .ok_or_else(|| InterpretError::new(format!("undefined variable {}", &name)))?
                    .1 = value;
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

pub fn example() -> IR<'static> {
    let add = Builtin {
        args: vec!["x", "y"],
        func: Box::new(|args| {
            let RValue::Number(x) = args[0] else {
                return Err(InterpretError::new("x is not a number"));
            };
            let RValue::Number(y) = args[1] else {
                return Err(InterpretError::new("y is not a number"));
            };
            Ok(RValue::Number(x + y))
        }),
    };

    IR {
        builtins: HashMap::from([("add".to_string(), add)]),
        root: Node {
            tree: Tree::Call {
                callee: Box::new(Node {
                    tree: Tree::Constant {
                        value: Value::LValue("add".to_string()),
                    },
                }),
                args: vec![
                    Node {
                        tree: Tree::Constant {
                            value: Value::RValue(RValue::Number(9)),
                        },
                    },
                    Node {
                        tree: Tree::Constant {
                            value: Value::RValue(RValue::Number(10)),
                        },
                    },
                ],
            },
        },
    }
}
