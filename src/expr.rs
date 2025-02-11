use core::{borrow::Borrow, iter::zip};

use alloc::{boxed::Box, string::String, vec::Vec};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Negate,
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
    Function(&'a Expr),
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

    fn unwrap_function(self) -> &'a Expr {
        match self {
            Value::Function(expr) => expr,
            _ => panic!("unwrap of non-function value {:?}", self),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Skip,                                   // Void
    Ignore(Box<Expr>),                      // Void
    Location(Pointer, String),              // Value
    Constant(i32),                          // Value
    Unary(UnaryOp, Box<Expr>),              // Value
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // Value
    Assign(Box<Expr>, Box<Expr>),           // Void
    Seq(Box<Expr>, Box<Expr>),              // any
    Let(String, Box<Expr>),                 // Void
    Scope(String, Box<Expr>, Box<Expr>),    // any
    If(Box<Expr>, Box<Expr>, Box<Expr>),    // any
    Function(Vec<String>, Box<Expr>),       // Value
    Call(Box<Expr>, Vec<Expr>),             // Value
}

#[derive(Default)]
struct Stack<'a>(Vec<(String, Value<'a>)>);

impl<'a> Stack<'a> {
    fn new() -> Self {
        Self::default()
    }

    // inverse indexation
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

impl Expr {
    pub fn new_seq(first_expr: Expr, second_expr: Expr) -> Expr {
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
        }
    }

    fn eval_impl<'a>(&'a self, stack: &mut Stack<'a>) -> Value<'a> {
        match self {
            Expr::Skip => Value::Void,
            Expr::Ignore(expr) => {
                expr.eval_impl(stack);
                Value::Void
            }
            Expr::Location(ptr, _) => stack.get(*ptr).cloned().unwrap_or(Value::Void),
            Expr::Constant(val) => Value::Number(val.clone()),
            Expr::Unary(op, arg_expr) => {
                if *op == UnaryOp::Reference {
                    if let Expr::Location(ptr, _) = arg_expr.as_ref() {
                        return ptr
                            .to_absolute(stack)
                            .map(|i| Value::Pointer(Pointer::Absolute(i)))
                            .unwrap_or(Value::Void);
                    }
                    panic!("getting address of arbitrary expression is not supported")
                }
                let arg = arg_expr.eval_impl(stack);
                match op {
                    UnaryOp::Negate => Value::Number(arg.unwrap_number().wrapping_neg()),
                    UnaryOp::Dereference => match arg {
                        Value::Pointer(i) => stack.get(i).cloned().unwrap_or(Value::Void),
                        _ => Value::Void,
                    },
                    UnaryOp::Reference => panic!("unreachable"),
                }
            }
            Expr::Binary(op, left_expr, right_expr) => {
                let left = left_expr.eval_impl(stack);
                let right = right_expr.eval_impl(stack);
                op.calc(left, right)
            }
            Expr::Assign(lhs_expr, rhs_expr) => {
                let lhs = lhs_expr.eval_impl(stack).unwrap_pointer();
                let rhs = rhs_expr.eval_impl(stack);
                stack.assign(lhs, rhs.clone());
                rhs
            }
            Expr::Seq(first_expr, second_expr) => {
                first_expr.eval_impl(stack);
                second_expr.eval_impl(stack)
            }
            Expr::Let(_, _) => {
                panic!("Let is an intermediate node and can't be present in final AST")
            }
            Expr::Scope(var, val_expr, cont_expr) => {
                let val = val_expr.eval_impl(stack);
                stack.unshift(var.clone(), val);
                let res = cont_expr.eval_impl(stack);
                stack.shift();
                res
            }
            Expr::If(cond_expr, true_expr, false_expr) => {
                let cond = cond_expr.eval_impl(stack).unwrap_boolean();
                if cond {
                    true_expr.eval_impl(stack)
                } else {
                    false_expr.eval_impl(stack)
                }
            }
            Expr::Function(_, _) => Value::Function(&self),
            Expr::Call(callee_expr, arg_exprs) => {
                let Expr::Function(arg_names, body_expr) =
                    callee_expr.eval_impl(stack).unwrap_function()
                else {
                    panic!("call on non-function value {:?}", callee_expr);
                };

                let arg_vals: Vec<_> = arg_exprs.iter().map(|expr| expr.eval_impl(stack)).collect();
                zip(arg_names.iter(), arg_vals.iter())
                    .for_each(|(name, val)| stack.unshift(name.clone(), val.clone()));
                let res = body_expr.eval_impl(stack);
                arg_exprs.iter().for_each(|_| {
                    stack.shift();
                });
                res
            }
        }
    }

    pub fn eval(&self) -> Value {
        let mut stack: Stack = Stack::new();
        self.eval_impl(&mut stack)
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
        }
    }

    pub fn set_up(&mut self) {
        let mut stack = Stack::new();
        self.set_up_impl(&mut stack);
    }
}
