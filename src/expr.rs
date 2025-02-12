use core::{borrow::Borrow, iter::zip};

use alloc::{boxed::Box, string::String, vec::Vec};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Negate,
    LogicNot,
    Reference,
    Dereference,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    LogicOr,
    LogicAnd,
}

impl BinaryOp {
    fn calc_num_to_num<'a, F: Fn(i32, i32) -> i32>(a: Value<'a>, b: Value<'a>, f: F) -> Value<'a> {
        Value::Number(f(a.unwrap_number(), b.unwrap_number()))
    }

    fn calc_num_to_bool<'a, F: Fn(&i32, &i32) -> bool>(
        a: Value<'a>,
        b: Value<'a>,
        f: F,
    ) -> Value<'a> {
        Value::Boolean(f(&a.unwrap_number(), &b.unwrap_number()))
    }

    fn calc_bool_to_bool<'a, F: Fn(bool, bool) -> bool>(
        a: Value<'a>,
        b: Value<'a>,
        f: F,
    ) -> Value<'a> {
        Value::Boolean(f(a.unwrap_boolean(), b.unwrap_boolean()))
    }

    pub fn calc<'a>(&self, a: Value<'a>, b: Value<'a>) -> Value<'a> {
        match self {
            Self::Add => Self::calc_num_to_num(a, b, i32::wrapping_add),
            Self::Subtract => Self::calc_num_to_num(a, b, i32::wrapping_sub),
            Self::Multiply => Self::calc_num_to_num(a, b, i32::wrapping_mul),
            Self::Divide => Self::calc_num_to_num(a, b, i32::wrapping_div),
            Self::Equal => Self::calc_num_to_bool(a, b, i32::eq),
            Self::NotEqual => Self::calc_num_to_bool(a, b, i32::ne),
            Self::LessEqual => Self::calc_num_to_bool(a, b, i32::le),
            Self::GreaterEqual => Self::calc_num_to_bool(a, b, i32::ge),
            Self::Less => Self::calc_num_to_bool(a, b, i32::lt),
            Self::Greater => Self::calc_num_to_bool(a, b, i32::gt),
            Self::LogicOr => Self::calc_bool_to_bool(a, b, |a, b| a || b),
            Self::LogicAnd => Self::calc_bool_to_bool(a, b, |a, b| a && b),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Pointer {
    Relative(usize),
    Absolute(usize),
    Invalid,
}

impl Pointer {
    fn to_absolute<'a>(self, stack: &Stack<'a>) -> Option<usize> {
        match self {
            Pointer::Relative(i) => Some(stack.0.len() - i - 1),
            Pointer::Absolute(i) => Some(i),
            Pointer::Invalid => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Number(i32),
    Boolean(bool),
    Pointer(Pointer),
    Function(&'a Expr<'a>),
    Void,
}

impl<'a> Value<'a> {
    fn unwrap_number(self) -> i32 {
        match self {
            Value::Number(val) => val,
            _ => panic!("unwrap of non-number value {:?}", self),
        }
    }

    fn unwrap_boolean(self) -> bool {
        match self {
            Value::Boolean(val) => val,
            _ => panic!("unwrap of non-boolean value {:?}", self),
        }
    }

    fn unwrap_pointer(self) -> Pointer {
        match self {
            Value::Pointer(ptr) => ptr,
            _ => panic!("unwrap of non-pointer value {:?}", self),
        }
    }

    fn unwrap_function(self) -> &'a Expr<'a> {
        match self {
            Value::Function(expr) => expr,
            _ => panic!("unwrap of non-function value {:?}", self),
        }
    }

    fn to_flow(self) -> Flow<'a> {
        Flow::Normal(self)
    }
}

enum Flow<'a, R = Value<'a>> {
    Normal(R),
    Return(Value<'a>),
    Break,
}

impl<'a, T> Flow<'a, T> {
    fn chain<R, F: FnOnce() -> Flow<'a, R>>(self, f: F) -> Flow<'a, (T, R)> {
        self.and_then(|first| match f() {
            Flow::Normal(second) => Flow::Normal((first, second)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Break => Flow::Break,
        })
    }

    fn and_then<R, F: FnOnce(T) -> Flow<'a, R>>(self, f: F) -> Flow<'a, R> {
        match self {
            Flow::Normal(val) => match f(val) {
                Flow::Normal(second) => Flow::Normal(second),
                Flow::Return(val) => Flow::Return(val),
                Flow::Break => Flow::Break,
            },
            Flow::Return(val) => Flow::Return(val),
            Flow::Break => Flow::Break,
        }
    }

    fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Flow<'a, R> {
        match self {
            Flow::Normal(val) => Flow::Normal(f(val)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Break => Flow::Break,
        }
    }
}

impl<'a> Flow<'a> {
    fn to_value(self) -> Value<'a> {
        match self {
            Flow::Normal(val) => val,
            Flow::Return(val) => val,
            Flow::Break => Value::Void,
        }
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Skip,                                   // Void
    Ignore(Box<Self>),                      // Void
    Location(Pointer, String),              // Value
    Constant(Value<'a>),                    // Value
    Unary(UnaryOp, Box<Self>),              // Value
    Binary(BinaryOp, Box<Self>, Box<Self>), // Value
    Assign(Box<Self>, Box<Self>),           // Void
    Seq(Box<Self>, Box<Self>),              // any
    Let(String, Box<Self>),                 // Void
    Scope(String, Box<Self>, Box<Self>),    // any
    If(Box<Self>, Box<Self>, Box<Self>),    // any
    Function(Vec<String>, Box<Self>),       // Value
    Call(Box<Self>, Vec<Self>),             // Value
    Return(Box<Self>),                      // Void
    While(Box<Self>, Box<Self>),            // Void
    Break,                                  // Void
}

#[derive(Default)]
struct Stack<'a>(Vec<(String, Value<'a>)>);

impl<'a> Stack<'a> {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, ptr: Pointer) -> Option<&Value<'a>> {
        let index_opt = ptr.to_absolute(&self);
        index_opt.and_then(|i| self.0.get(i)).map(|e| &e.1)
    }

    fn index<Q>(&self, k: &Q) -> Option<usize>
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0.iter().rev().position(|(var, _)| var.borrow() == k)
    }

    fn assign(&mut self, ptr: Pointer, v: Value<'a>) {
        let index_opt = ptr.to_absolute(&self);
        index_opt.and_then(|i| self.0.get_mut(i)).map(|e| e.1 = v);
    }

    fn unshift(&mut self, k: String, v: Value<'a>) {
        self.0.push((k, v));
    }

    fn shift(&mut self) -> Option<(String, Value<'a>)> {
        self.0.pop()
    }
}

impl<'a> Expr<'a> {
    pub fn new_seq(first_expr: Self, second_expr: Self) -> Self {
        match (first_expr, second_expr) {
            (Expr::Let(var, val_expr), inner_expr) => {
                Expr::Scope(var, val_expr, Box::new(inner_expr))
            }
            (Expr::Skip, second_expr) => second_expr,
            (first_expr, second_expr) => Expr::Seq(
                if first_expr.is_value() {
                    Box::new(Expr::Ignore(Box::new(first_expr)))
                } else {
                    Box::new(first_expr)
                },
                Box::new(second_expr),
            ),
        }
    }

    fn is_value(&self) -> bool {
        // TODO: maybe optimize
        match self {
            Expr::Skip => false,
            Expr::Ignore(_) => false,
            Expr::Location(_, _) => true,
            Expr::Constant(_) => true,
            Expr::Unary(_, _) => true,
            Expr::Binary(_, _, _) => true,
            Expr::Assign(_, _) => false,
            Expr::Seq(_, second_expr) => second_expr.is_value(),
            Expr::Let(_, _) => false,
            Expr::Scope(_, _, cont_expr) => cont_expr.is_value(),
            Expr::If(_, _, false_expr) => false_expr.is_value(),
            Expr::Function(_, _) => true,
            Expr::Call(_, _) => true,
            Expr::Return(_) => false,
            Expr::While(_, _) => false,
            Expr::Break => false,
        }
    }

    fn eval_impl(&'a self, stack: &mut Stack<'a>) -> Flow<'a> {
        match self {
            Expr::Skip => Flow::Normal(Value::Void),
            Expr::Ignore(expr) => expr.eval_impl(stack).map(|_| Value::Void),
            Expr::Location(ptr, _) => stack.get(*ptr).cloned().unwrap_or(Value::Void).to_flow(),
            Expr::Constant(val) => val.clone().to_flow(),
            Expr::Unary(op, arg_expr) => {
                if *op == UnaryOp::Reference {
                    if let Expr::Location(ptr, _) = arg_expr.as_ref() {
                        return ptr
                            .to_absolute(stack)
                            .map(|i| Value::Pointer(Pointer::Absolute(i)))
                            .unwrap_or(Value::Void)
                            .to_flow();
                    }
                    panic!("getting address of arbitrary expression is not supported")
                }
                arg_expr.eval_impl(stack).map(|arg| match op {
                    UnaryOp::Negate => Value::Number(arg.unwrap_number().wrapping_neg()),
                    UnaryOp::LogicNot => Value::Boolean(!arg.unwrap_boolean()),
                    UnaryOp::Dereference => match arg {
                        Value::Pointer(i) => stack.get(i).cloned().unwrap_or(Value::Void),
                        _ => Value::Void,
                    },
                    UnaryOp::Reference => panic!("unreachable"),
                })
            }
            Expr::Binary(op, left_expr, right_expr) => left_expr
                .eval_impl(stack)
                .chain(|| right_expr.eval_impl(stack))
                .map(|(left, right)| op.calc(left, right)),
            Expr::Assign(lhs_expr, rhs_expr) => lhs_expr
                .eval_impl(stack)
                .chain(|| rhs_expr.eval_impl(stack))
                .map(|(left, right)| {
                    stack.assign(left.unwrap_pointer(), right.clone());
                    right
                }),
            Expr::Seq(first_expr, second_expr) => first_expr
                .eval_impl(stack)
                .and_then(|_| second_expr.eval_impl(stack)),
            Expr::Let(_, _) => {
                panic!("Let is an intermediate node and can't be present in final AST")
            }
            Expr::Scope(var, val_expr, cont_expr) => val_expr.eval_impl(stack).and_then(|val| {
                stack.unshift(var.clone(), val);
                let res = cont_expr.eval_impl(stack);
                stack.shift();
                res
            }),
            Expr::If(cond_expr, true_expr, false_expr) => {
                cond_expr.eval_impl(stack).and_then(|cond| {
                    if cond.unwrap_boolean() {
                        true_expr.eval_impl(stack)
                    } else {
                        false_expr.eval_impl(stack)
                    }
                })
            }
            Expr::Function(_, _) => Value::Function(&self).to_flow(),
            Expr::Call(callee_expr, arg_exprs) => callee_expr
                .eval_impl(stack)
                .map(|func| {
                    let Expr::Function(arg_names, body_expr) = func.unwrap_function() else {
                        panic!("call on non-function value {:?}", callee_expr);
                    };
                    (arg_names, body_expr)
                })
                .and_then(|(arg_names, body_expr)| {
                    arg_exprs
                        .iter()
                        .fold(Flow::Normal(Vec::new()), |vals, expr| {
                            vals.and_then(|mut vals| {
                                expr.eval_impl(stack).map(|val| {
                                    vals.push(val);
                                    vals
                                })
                            })
                        })
                        .map(|arg_vals| {
                            zip(arg_names.iter(), arg_vals.iter())
                                .for_each(|(name, val)| stack.unshift(name.clone(), val.clone()));
                            let res = body_expr.eval_impl(stack);
                            arg_names.iter().for_each(|_| {
                                stack.shift();
                            });
                            res.to_value()
                        })
                }),
            Expr::Return(expr) => expr.eval_impl(stack).and_then(|val| Flow::Return(val)),
            Expr::While(cond_expr, inner_expr) => loop {
                let flow = cond_expr
                    .eval_impl(stack)
                    .and_then(|cond| {
                        if cond.unwrap_boolean() {
                            Flow::Normal(())
                        } else {
                            Flow::Break
                        }
                    })
                    .and_then(|_| inner_expr.eval_impl(stack));
                match flow {
                    Flow::Normal(_) => {}
                    Flow::Return(_) => break flow,
                    Flow::Break => break Value::Void.to_flow(),
                }
            },
            Expr::Break => Flow::Break,
        }
    }

    pub fn eval(&self) -> Value {
        let mut stack: Stack = Stack::new();
        self.eval_impl(&mut stack).to_value()
    }

    fn set_up_impl(&mut self, stack: &mut Stack) {
        match self {
            Expr::Skip => {}
            Expr::Ignore(expr) => expr.set_up_impl(stack),
            Expr::Location(ptr, var) => {
                *ptr = Pointer::Relative(
                    stack
                        .index(var)
                        .unwrap_or_else(|| panic!("undefined variable {}", var)),
                );
            }
            Expr::Constant(_) => {}
            Expr::Unary(_, arg_expr) => arg_expr.set_up_impl(stack),
            Expr::Binary(_, left_expr, right_expr) => {
                left_expr.set_up_impl(stack);
                right_expr.set_up_impl(stack);
            }
            Expr::Assign(left_expr, right_expr) => {
                left_expr.set_up_impl(stack);
                right_expr.set_up_impl(stack);
            }
            Expr::Seq(first_expr, second_expr) => {
                first_expr.set_up_impl(stack);
                second_expr.set_up_impl(stack);
            }
            Expr::Let(_, expr) => expr.set_up_impl(stack),
            Expr::Scope(var, val_expr, cont_expr) => {
                val_expr.set_up_impl(stack);
                stack.unshift(var.clone(), Value::Void);
                cont_expr.set_up_impl(stack);
                stack.shift();
            }
            Expr::If(cond_expr, true_expr, false_expr) => {
                cond_expr.set_up_impl(stack);
                true_expr.set_up_impl(stack);
                false_expr.set_up_impl(stack);
            }
            Expr::Function(args, body_expr) => {
                args.iter().for_each(|var| {
                    stack.unshift(var.clone(), Value::Void);
                });
                body_expr.set_up_impl(stack);
                args.iter().for_each(|_| {
                    stack.shift();
                })
            }
            Expr::Call(callee_expr, arg_exprs) => {
                callee_expr.set_up_impl(stack);
                arg_exprs
                    .iter_mut()
                    .for_each(|arg_expr| arg_expr.set_up_impl(stack));
            }
            Expr::Return(expr) => expr.set_up_impl(stack),
            Expr::While(cond_expr, inner_expr) => {
                cond_expr.set_up_impl(stack);
                inner_expr.set_up_impl(stack);
            }
            Expr::Break => {}
        }
    }

    pub fn set_up(&mut self) {
        let mut stack = Stack::new();
        self.set_up_impl(&mut stack);
    }
}
