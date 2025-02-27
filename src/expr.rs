use core::{
    borrow::Borrow,
    cell::Cell,
    iter::zip,
    mem,
    ops::{Deref, Range, RangeInclusive},
};

use alloc::{boxed::Box, rc::Rc, vec::Vec};

use crate::span::Spanned;

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
    Modulo,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    LogicOr,
    LogicAnd,
    Range,
    RangeInclusive,
}

impl BinaryOp {
    fn calc_num_to_num<'src, F: FnOnce(i32, i32) -> i32>(a: Value, b: Value, f: F) -> Value<'src> {
        Value::Number(f(a.unwrap_number(), b.unwrap_number()))
    }

    fn calc_num_to_bool<'src, F: FnOnce(&i32, &i32) -> bool>(
        a: Value,
        b: Value,
        f: F,
    ) -> Value<'src> {
        Value::Boolean(f(&a.unwrap_number(), &b.unwrap_number()))
    }

    fn calc_num_to_any<'src, T, F: FnOnce(i32, i32) -> T, G: Fn(T) -> Value<'src>>(
        a: Value,
        b: Value,
        f: F,
        cons: G,
    ) -> Value<'src> {
        cons(f(a.unwrap_number(), b.unwrap_number()))
    }

    fn calc_bool_to_bool<'src, F: FnOnce(bool, bool) -> bool>(
        a: Value<'src>,
        b: Value<'src>,
        f: F,
    ) -> Value<'src> {
        Value::Boolean(f(a.unwrap_boolean(), b.unwrap_boolean()))
    }

    pub fn calc<'src>(&self, a: Value<'src>, b: Value<'src>) -> Value<'src> {
        match self {
            Self::Add => Self::calc_num_to_num(a, b, i32::wrapping_add),
            Self::Subtract => Self::calc_num_to_num(a, b, i32::wrapping_sub),
            Self::Multiply => Self::calc_num_to_num(a, b, i32::wrapping_mul),
            Self::Divide => Self::calc_num_to_num(a, b, i32::wrapping_div),
            Self::Modulo => Self::calc_num_to_num(a, b, i32::wrapping_rem),
            Self::Equal => Self::calc_num_to_bool(a, b, i32::eq),
            Self::NotEqual => Self::calc_num_to_bool(a, b, i32::ne),
            Self::LessEqual => Self::calc_num_to_bool(a, b, i32::le),
            Self::GreaterEqual => Self::calc_num_to_bool(a, b, i32::ge),
            Self::Less => Self::calc_num_to_bool(a, b, i32::lt),
            Self::Greater => Self::calc_num_to_bool(a, b, i32::gt),
            Self::LogicOr => Self::calc_bool_to_bool(a, b, |a, b| a || b),
            Self::LogicAnd => Self::calc_bool_to_bool(a, b, |a, b| a && b),
            Self::Range => Self::calc_num_to_any(a, b, |a, b| a..b, Value::Range),
            Self::RangeInclusive => {
                Self::calc_num_to_any(a, b, |a, b| a..=b, Value::RangeInclusive)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Pointer {
    Relative(usize),
    Absolute(usize),
    Function(usize),
    Invalid,
}

impl Pointer {
    fn to_absolute(self, stack: &Stack) -> Option<usize> {
        match self {
            Pointer::Relative(i) => Some(stack.0.len() - i - 1),
            Pointer::Absolute(i) => Some(i),
            Pointer::Function(_) => None,
            Pointer::Invalid => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'src> {
    Number(i32),
    Boolean(bool),
    Range(Range<i32>),
    RangeInclusive(RangeInclusive<i32>),
    Pointer(Pointer),
    Function(usize),
    Closure(Rc<Function<'src>>),
    Void,
}

impl<'src> Default for Value<'src> {
    fn default() -> Self {
        Value::Void
    }
}

impl<'src> Value<'src> {
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

    fn unwrap_range(self) -> Box<dyn Iterator<Item = i32>> {
        match self {
            Value::Range(range) => Box::new(range),
            Value::RangeInclusive(range) => Box::new(range),
            _ => panic!("unwrap of non-range value {:?}", self),
        }
    }

    fn unwrap_pointer(self) -> Pointer {
        match self {
            Value::Pointer(ptr) => ptr,
            _ => panic!("unwrap of non-pointer value {:?}", self),
        }
    }

    fn unwrap_function(self, env: &Environment<'src>) -> Rc<Function<'src>> {
        match self {
            Value::Function(i) => env.functions.get(i).unwrap().clone(),
            Value::Closure(func) => func,
            _ => panic!("unwrap of non-function value {:?}", self),
        }
    }

    fn to_flow(self) -> Flow<'src> {
        Flow::Normal(self)
    }
}

enum Flow<'src, R = Value<'src>> {
    Normal(R),
    Return(Value<'src>),
    Continue,
    Break,
}

impl<'src, T: Default> Default for Flow<'src, T> {
    fn default() -> Self {
        Flow::Normal(T::default())
    }
}

impl<'src, T> Flow<'src, T> {
    fn chain<R, F: FnOnce() -> Flow<'src, R>>(self, f: F) -> Flow<'src, (T, R)> {
        self.and_then(|first| match f() {
            Flow::Normal(second) => Flow::Normal((first, second)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Continue => Flow::Continue,
            Flow::Break => Flow::Break,
        })
    }

    fn and_then<R, F: FnOnce(T) -> Flow<'src, R>>(self, f: F) -> Flow<'src, R> {
        match self {
            Flow::Normal(val) => match f(val) {
                Flow::Normal(second) => Flow::Normal(second),
                Flow::Return(val) => Flow::Return(val),
                Flow::Continue => Flow::Continue,
                Flow::Break => Flow::Break,
            },
            Flow::Return(val) => Flow::Return(val),
            Flow::Continue => Flow::Continue,
            Flow::Break => Flow::Break,
        }
    }

    fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Flow<'src, R> {
        match self {
            Flow::Normal(val) => Flow::Normal(f(val)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Continue => Flow::Continue,
            Flow::Break => Flow::Break,
        }
    }
}

impl<'src> Flow<'src> {
    fn to_value(self) -> Value<'src> {
        match self {
            Flow::Normal(val) => val,
            Flow::Return(val) => val,
            Flow::Continue => Value::Void,
            Flow::Break => Value::Void,
        }
    }
}

#[derive(Debug)]
pub struct Function<'src>(pub Vec<Spanned<&'src str>>, pub Box<Spanned<Expr<'src>>>);

#[derive(Debug)]
pub enum Expr<'src> {
    Skip,                                                                 // Void
    Block(Box<Spanned<Self>>),                                            // any
    Ignore(Box<Spanned<Self>>),                                           // Void
    Location(Cell<Pointer>, &'src str),                                   // Value
    Constant(Value<'src>),                                                // Value
    Unary(UnaryOp, Box<Spanned<Self>>),                                   // Value
    Binary(BinaryOp, Box<Spanned<Self>>, Box<Spanned<Self>>),             // Value
    Assign(Box<Spanned<Self>>, Box<Spanned<Self>>),                       // Void
    Seq(Box<Spanned<Self>>, Box<Spanned<Self>>),                          // any
    Let(Spanned<&'src str>, Box<Spanned<Self>>),                          // Void
    VarScope(Spanned<&'src str>, Box<Spanned<Self>>, Box<Spanned<Self>>), // any
    Function(Spanned<&'src str>, Rc<Function<'src>>),                     // Void
    FunScope(Spanned<&'src str>, Rc<Function<'src>>, Box<Spanned<Self>>), // any
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),       // any
    Closure(Rc<Function<'src>>),                                          // Value
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),                         // Value
    Return(Box<Spanned<Self>>),                                           // Void
    While(Box<Spanned<Self>>, Box<Spanned<Self>>),                        // Void
    For(Spanned<&'src str>, Box<Spanned<Self>>, Box<Spanned<Self>>),      // Void
    Continue,                                                             // Void
    Break,                                                                // Void
}

impl<'src> Default for Expr<'src> {
    fn default() -> Self {
        Expr::Skip
    }
}

#[derive(Default)]
struct Stack<'src>(Vec<(&'src str, Value<'src>)>);

#[derive(Default)]
pub struct Environment<'src> {
    stack: Stack<'src>,
    functions: Vec<Rc<Function<'src>>>,
}

impl<'src> Stack<'src> {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, ptr: Pointer) -> Option<&Value<'src>> {
        let index_opt = ptr.to_absolute(&self);
        index_opt.and_then(|i| self.0.get(i)).map(|e| &e.1)
    }

    fn index<Q>(&self, k: &Q) -> Option<usize>
    where
        &'src str: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0.iter().rev().position(|(var, _)| var.borrow() == k)
    }

    fn assign(&mut self, ptr: Pointer, v: Value<'src>) {
        let index_opt = ptr.to_absolute(&self);
        index_opt.and_then(|i| self.0.get_mut(i)).map(|e| e.1 = v);
    }

    fn push(&mut self, k: &'src str, v: Value<'src>) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(&'src str, Value<'src>)> {
        self.0.pop()
    }
}

impl<'src> Environment<'src> {
    fn get(&self, ptr: Pointer) -> Option<Value<'src>> {
        self.stack.get(ptr).cloned().or_else(|| match ptr {
            Pointer::Function(i) => Some(Value::Function(i)),
            _ => None,
        })
    }
}

impl<'src> Expr<'src> {
    pub fn new_ignore(inner_span: Spanned<Self>) -> Spanned<Self> {
        if inner_span.is_value() {
            let span = inner_span.span();
            Spanned(Expr::Ignore(Box::new(inner_span)), span)
        } else {
            inner_span
        }
    }

    pub fn new_seq(first_span: Spanned<Self>, second_span: Spanned<Self>) -> Self {
        match first_span.0 {
            Expr::Let(var, val_expr) => Expr::VarScope(var, val_expr, Box::new(second_span)),
            Expr::Function(name, func) => Expr::FunScope(name, func, Box::new(second_span)),
            Expr::Skip => second_span.0,
            _ => Expr::Seq(
                Box::new(Self::new_ignore(first_span)),
                Box::new(second_span),
            ),
        }
    }

    fn is_value(&self) -> bool {
        // TODO: maybe optimize
        match self {
            Expr::Skip => false,
            Expr::Block(expr) => expr.is_value(),
            Expr::Ignore(_) => false,
            Expr::Location(_, _) => true,
            Expr::Constant(_) => true,
            Expr::Unary(_, _) => true,
            Expr::Binary(_, _, _) => true,
            Expr::Assign(_, _) => false,
            Expr::Seq(_, second_expr) => second_expr.is_value(),
            Expr::Let(_, _) => false,
            Expr::VarScope(_, _, cont_expr) => cont_expr.is_value(),
            Expr::Function(_, _) => false,
            Expr::FunScope(_, _, cont_expr) => cont_expr.is_value(),
            Expr::If(_, _, false_expr) => false_expr.is_value(),
            Expr::Closure(_) => true,
            Expr::Call(_, _) => true,
            Expr::Return(_) => false,
            Expr::While(_, _) => false,
            Expr::For(_, _, _) => false,
            Expr::Continue => false,
            Expr::Break => false,
        }
    }

    pub fn walk<F: FnMut(&Spanned<Self>) -> bool>(spanned_self: &Spanned<Self>, mut walker: F) -> F {
        let cont = walker(spanned_self);
        if !cont {
            return walker;
        }
        match &spanned_self.0 {
            Expr::Skip => {}
            Expr::Block(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Ignore(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Location(_, _) => {}
            Expr::Constant(_) => {}
            Expr::Unary(_, arg_expr) => {
                walker = Self::walk(arg_expr.as_ref(), walker);
            }
            Expr::Binary(_, left_expr, right_expr) => {
                walker = Self::walk(left_expr.as_ref(), walker);
                walker = Self::walk(right_expr.as_ref(), walker);
            }
            Expr::Assign(left_expr, right_expr) => {
                walker = Self::walk(left_expr.as_ref(), walker);
                walker = Self::walk(right_expr.as_ref(), walker);
            }
            Expr::Seq(first_expr, second_expr) => {
                walker = Self::walk(first_expr.as_ref(), walker);
                walker = Self::walk(second_expr.as_ref(), walker);
            }
            Expr::Let(_, expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::VarScope(_, val_expr, cont_expr) => {
                walker = Self::walk(val_expr.as_ref(), walker);
                walker = Self::walk(cont_expr.as_ref(), walker);
            }
            Expr::Function(_, func) => {
                let Function(_, body_expr) = func.deref();
                walker = Self::walk(body_expr.as_ref(), walker);
            }
            Expr::FunScope(_, func, cont_expr) => {
                let Function(_, body_expr) = func.deref();
                walker = Self::walk(body_expr.as_ref(), walker);
                walker = Self::walk(cont_expr.as_ref(), walker);
            }
            Expr::If(cond_expr, true_expr, false_expr) => {
                walker = Self::walk(cond_expr.as_ref(), walker);
                walker = Self::walk(true_expr.as_ref(), walker);
                walker = Self::walk(false_expr.as_ref(), walker);
            }
            Expr::Closure(func) => {
                let Function(_, body_expr) = func.deref();
                walker = Self::walk(body_expr.as_ref(), walker);
            }
            Expr::Call(callee_expr, arg_exprs) => {
                walker = Self::walk(callee_expr.as_ref(), walker);
                for arg_expr in arg_exprs.iter() {
                    walker = Self::walk(arg_expr, walker)
                }
            }
            Expr::Return(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::While(cond_expr, inner_expr) => {
                walker = Self::walk(cond_expr.as_ref(), walker);
                walker = Self::walk(inner_expr.as_ref(), walker);
            }
            Expr::For(_, iter_expr, inner_expr) => {
                walker = Self::walk(iter_expr.as_ref(), walker);
                walker = Self::walk(inner_expr.as_ref(), walker);
            }
            Expr::Continue => {}
            Expr::Break => {}
        };
        walker
    }

    fn eval_impl(&self, env: &mut Environment<'src>) -> Flow<'src> {
        match self {
            Expr::Skip => Flow::Normal(Value::Void),
            Expr::Block(expr) => expr.eval_impl(env),
            Expr::Ignore(expr) => expr.eval_impl(env).map(|_| Value::Void),
            Expr::Location(ptr, _) => env.get(ptr.get()).unwrap_or(Value::Void).to_flow(),
            Expr::Constant(val) => val.clone().to_flow(),
            Expr::Unary(op, arg_expr) => {
                if *op == UnaryOp::Reference {
                    if let Expr::Location(ptr, _) = arg_expr.as_ref().deref() {
                        return match ptr.get() {
                            Pointer::Function(i) => Value::Pointer(Pointer::Function(i)),
                            ptr => ptr
                                .to_absolute(&mut env.stack)
                                .map(|i| Value::Pointer(Pointer::Absolute(i)))
                                .unwrap_or(Value::Void),
                        }
                        .to_flow();
                    }
                    panic!("getting address of arbitrary expression is not supported")
                }
                arg_expr.eval_impl(env).map(|arg| match op {
                    UnaryOp::Negate => Value::Number(arg.unwrap_number().wrapping_neg()),
                    UnaryOp::LogicNot => Value::Boolean(!arg.unwrap_boolean()),
                    UnaryOp::Dereference => match arg {
                        Value::Pointer(i) => env.get(i).unwrap_or(Value::Void),
                        _ => Value::Void,
                    },
                    UnaryOp::Reference => unreachable!(),
                })
            }
            Expr::Binary(op, left_expr, right_expr) => left_expr
                .eval_impl(env)
                .chain(|| right_expr.eval_impl(env))
                .map(|(left, right)| op.calc(left, right)),
            Expr::Assign(lhs_expr, rhs_expr) => lhs_expr
                .eval_impl(env)
                .chain(|| rhs_expr.eval_impl(env))
                .map(|(left, right)| {
                    env.stack.assign(left.unwrap_pointer(), right.clone());
                    right
                }),
            Expr::Seq(first_expr, second_expr) => first_expr
                .eval_impl(env)
                .and_then(|_| second_expr.eval_impl(env)),
            Expr::Let(_, _) => unreachable!(),
            Expr::VarScope(var, val_expr, cont_expr) => val_expr.eval_impl(env).and_then(|val| {
                env.stack.push(var, val);
                let res = cont_expr.eval_impl(env);
                env.stack.pop();
                res
            }),
            Expr::Function(_, _) => unreachable!(),
            Expr::FunScope(_, _, cont_expr) => cont_expr.eval_impl(env),
            Expr::If(cond_expr, true_expr, false_expr) => {
                cond_expr.eval_impl(env).and_then(|cond| {
                    if cond.unwrap_boolean() {
                        true_expr.eval_impl(env)
                    } else {
                        false_expr.eval_impl(env)
                    }
                })
            }
            Expr::Closure(func) => Value::Closure(func.clone()).to_flow(),
            Expr::Call(callee_expr, arg_exprs) => callee_expr
                .eval_impl(env)
                .map(|val| val.unwrap_function(env))
                .and_then(|func| {
                    let Function(arg_names, body_expr) = func.deref();
                    arg_exprs
                        .iter()
                        .fold(Flow::Normal(Vec::new()), |vals, expr| {
                            vals.and_then(|mut vals| {
                                expr.eval_impl(env).map(|val| {
                                    vals.push(val);
                                    vals
                                })
                            })
                        })
                        .map(|arg_vals| {
                            zip(arg_names.iter(), arg_vals.iter())
                                .for_each(|(name, val)| env.stack.push(name, val.clone()));
                            let res = body_expr.eval_impl(env);
                            arg_names.iter().for_each(|_| {
                                env.stack.pop();
                            });
                            res.to_value()
                        })
                }),
            Expr::Return(expr) => expr.eval_impl(env).and_then(|val| Flow::Return(val)),
            Expr::While(cond_expr, inner_expr) => loop {
                let flow = cond_expr
                    .eval_impl(env)
                    .and_then(|cond| {
                        if cond.unwrap_boolean() {
                            Flow::default()
                        } else {
                            Flow::Break
                        }
                    })
                    .and_then(|_: ()| inner_expr.eval_impl(env));
                match flow {
                    Flow::Return(_) => break flow,
                    Flow::Break => break Flow::default(),
                    _ => continue,
                }
            },
            Expr::For(var, iter_expr, inner_expr) => iter_expr.eval_impl(env).and_then(|val| {
                let flow = val.unwrap_range().into_iter().fold(
                    Value::Void.to_flow(),
                    |flow: Flow, i: i32| {
                        flow.and_then(|_| {
                            env.stack.push(var, Value::Number(i));
                            let res = match inner_expr.eval_impl(env) {
                                Flow::Continue => Flow::default(),
                                flow => flow,
                            };
                            env.stack.pop();
                            res
                        })
                    },
                );
                match flow {
                    Flow::Break => Flow::default(),
                    _ => flow,
                }
            }),
            Expr::Continue => Flow::Continue,
            Expr::Break => Flow::Break,
        }
    }

    pub fn eval(&self, mut env: Environment<'src>) -> Value<'src> {
        let res = self.eval_impl(&mut env).to_value();
        drop(env);
        res
    }

    fn set_up_impl(
        spanned_self: &Spanned<Self>,
        env: &mut Environment<'src>,
        sym_stack: &mut Vec<(&'src str, Option<usize>)>,
    ) {
        let _ = Expr::walk(spanned_self, |expr| match &expr.0 {
            Expr::Location(ptr, var) => {
                let opt = sym_stack
                    .iter()
                    .find(|(var2, _)| var == var2)
                    .map(|x| x.1)
                    .unwrap_or_else(|| panic!("undefined variable {}", var));
                ptr.set(match opt {
                    Some(i) => Pointer::Function(i),
                    None => Pointer::Relative(env.stack.index(var).unwrap_or_else(|| {
                        panic!(
                            "accessing variable {} outside of closure is not supported",
                            var
                        )
                    })),
                });
                true
            }
            Expr::VarScope(var, val_expr, cont_expr) => {
                Self::set_up_impl(val_expr.as_ref(), env, sym_stack);
                sym_stack.push((var.0, None));
                env.stack.push(var.0, Value::Void);
                Self::set_up_impl(cont_expr.as_ref(), env, sym_stack);
                env.stack.pop();
                false
            }
            Expr::FunScope(name, func, cont_expr) => {
                // disallow accessing variables outside of closure
                let Function(arg_names, body_expr) = func.deref();
                sym_stack.push((name.0, Some(env.functions.len())));
                env.functions.push(func.clone());
                let mut new_stack = Stack::new();
                for arg_name in arg_names.iter() {
                    new_stack.push(arg_name, Value::Void);
                    sym_stack.push((arg_name, None));
                }
                mem::swap(&mut env.stack, &mut new_stack);
                Self::set_up_impl(body_expr.as_ref(), env, sym_stack);
                mem::swap(&mut env.stack, &mut new_stack);
                for _ in arg_names.iter() {
                    sym_stack.pop();
                }
                Self::set_up_impl(cont_expr.as_ref(), env, sym_stack);
                sym_stack.pop();
                false
            }
            Expr::Closure(func) => {
                // disallow accessing variables outside of closure
                let Function(arg_names, body_expr) = func.deref();
                let mut new_stack = Stack::new();
                for arg_name in arg_names.iter() {
                    new_stack.push(arg_name, Value::Void);
                    sym_stack.push((arg_name, None));
                }
                mem::swap(&mut env.stack, &mut new_stack);
                Self::set_up_impl(body_expr.as_ref(), env, sym_stack);
                mem::swap(&mut env.stack, &mut new_stack);
                for _ in arg_names.iter() {
                    sym_stack.pop();
                }
                false
            }
            Expr::For(var, iter_expr, inner_expr) => {
                Self::set_up_impl(iter_expr.as_ref(), env, sym_stack);
                sym_stack.push((var.0, None));
                env.stack.push(var.0, Value::Void);
                Self::set_up_impl(inner_expr.as_ref(), env, sym_stack);
                env.stack.pop();
                false
            }
            _ => true,
        });
    }

    pub fn set_up(spanned_self: &Spanned<Self>) -> Environment<'src> {
        let mut env: Environment<'_> = Environment::default();
        let mut sym_stack: Vec<(&'src str, Option<usize>)> = Vec::new();
        Self::set_up_impl(spanned_self, &mut env, &mut sym_stack);
        env
    }
}
