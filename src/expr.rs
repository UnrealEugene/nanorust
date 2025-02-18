use core::{
    borrow::Borrow,
    cell::Cell,
    iter::zip,
    mem,
    ops::{Deref, Range, RangeInclusive},
};

use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};

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
    fn calc_num_to_num<F: FnOnce(i32, i32) -> i32>(a: Value, b: Value, f: F) -> Value {
        Value::Number(f(a.unwrap_number(), b.unwrap_number()))
    }

    fn calc_num_to_bool<F: FnOnce(&i32, &i32) -> bool>(a: Value, b: Value, f: F) -> Value {
        Value::Boolean(f(&a.unwrap_number(), &b.unwrap_number()))
    }

    fn calc_num_to_any<T, F: FnOnce(i32, i32) -> T, G: Fn(T) -> Value>(
        a: Value,
        b: Value,
        f: F,
        cons: G,
    ) -> Value {
        cons(f(a.unwrap_number(), b.unwrap_number()))
    }

    fn calc_bool_to_bool<F: FnOnce(bool, bool) -> bool>(a: Value, b: Value, f: F) -> Value {
        Value::Boolean(f(a.unwrap_boolean(), b.unwrap_boolean()))
    }

    pub fn calc(&self, a: Value, b: Value) -> Value {
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
pub enum Value {
    Number(i32),
    Boolean(bool),
    Range(Range<i32>),
    RangeInclusive(RangeInclusive<i32>),
    Pointer(Pointer),
    Function(usize),
    Closure(Rc<Function>),
    Void,
}

impl Default for Value {
    fn default() -> Self {
        Value::Void
    }
}

impl Value {
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

    fn unwrap_function(self, env: &Environment) -> Rc<Function> {
        match self {
            Value::Function(i) => env.functions.get(i).unwrap().clone(),
            Value::Closure(func) => func,
            _ => panic!("unwrap of non-function value {:?}", self),
        }
    }

    fn to_flow(self) -> Flow {
        Flow::Normal(self)
    }
}

enum Flow<R = Value> {
    Normal(R),
    Return(Value),
    Continue,
    Break,
}

impl<T: Default> Default for Flow<T> {
    fn default() -> Self {
        Flow::Normal(T::default())
    }
}

impl<T> Flow<T> {
    fn chain<R, F: FnOnce() -> Flow<R>>(self, f: F) -> Flow<(T, R)> {
        self.and_then(|first| match f() {
            Flow::Normal(second) => Flow::Normal((first, second)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Continue => Flow::Continue,
            Flow::Break => Flow::Break,
        })
    }

    fn and_then<R, F: FnOnce(T) -> Flow<R>>(self, f: F) -> Flow<R> {
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

    fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Flow<R> {
        match self {
            Flow::Normal(val) => Flow::Normal(f(val)),
            Flow::Return(val) => Flow::Return(val),
            Flow::Continue => Flow::Continue,
            Flow::Break => Flow::Break,
        }
    }
}

impl Flow {
    fn to_value(self) -> Value {
        match self {
            Flow::Normal(val) => val,
            Flow::Return(val) => val,
            Flow::Continue => Value::Void,
            Flow::Break => Value::Void,
        }
    }
}

#[derive(Debug)]
pub struct Function(pub Vec<String>, pub Box<Expr>);

#[derive(Debug)]
pub enum Expr {
    Skip,                                      // Void
    Ignore(Box<Self>),                         // Void
    Location(Cell<Pointer>, String),           // Value
    Constant(Value),                           // Value
    Unary(UnaryOp, Box<Self>),                 // Value
    Binary(BinaryOp, Box<Self>, Box<Self>),    // Value
    Assign(Box<Self>, Box<Self>),              // Void
    Seq(Box<Self>, Box<Self>),                 // any
    Let(String, Box<Self>),                    // Void
    VarScope(String, Box<Self>, Box<Self>),    // any
    Function(String, Rc<Function>),            // Void
    FunScope(String, Rc<Function>, Box<Self>), // any
    If(Box<Self>, Box<Self>, Box<Self>),       // any
    Closure(Rc<Function>),                     // Value
    Call(Box<Self>, Vec<Self>),                // Value
    Return(Box<Self>),                         // Void
    While(Box<Self>, Box<Self>),               // Void
    For(String, Box<Self>, Box<Self>),         // Void
    Continue,                                  // Void
    Break,                                     // Void
}

#[derive(Default)]
struct Stack(Vec<(String, Value)>);

#[derive(Default)]
pub struct Environment {
    stack: Stack,
    functions: Vec<Rc<Function>>,
}

impl Stack {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, ptr: Pointer) -> Option<&Value> {
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

    fn assign(&mut self, ptr: Pointer, v: Value) {
        let index_opt = ptr.to_absolute(&self);
        index_opt.and_then(|i| self.0.get_mut(i)).map(|e| e.1 = v);
    }

    fn push(&mut self, k: String, v: Value) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(String, Value)> {
        self.0.pop()
    }
}

impl Environment {
    fn get(&self, ptr: Pointer) -> Option<Value> {
        self.stack.get(ptr).cloned().or_else(|| match ptr {
            Pointer::Function(i) => Some(Value::Function(i)),
            _ => None,
        })
    }
}

impl Expr {
    pub fn new_ignore(inner_expr: Self) -> Self {
        if inner_expr.is_value() {
            Expr::Ignore(Box::new(inner_expr))
        } else {
            inner_expr
        }
    }

    pub fn new_seq(first_expr: Self, second_expr: Self) -> Self {
        match (first_expr, second_expr) {
            (Expr::Let(var, val_expr), inner_expr) => {
                Expr::VarScope(var, val_expr, Box::new(inner_expr))
            }
            (Expr::Function(name, func), inner_expr) => {
                Expr::FunScope(name, func, Box::new(inner_expr))
            }
            (Expr::Skip, second_expr) => second_expr,
            (first_expr, second_expr) => Expr::Seq(
                Box::new(Self::new_ignore(first_expr)),
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

    fn walk<F: FnMut(&Self) -> bool>(&self, mut walker: F) -> F {
        let cont = walker(self);
        if !cont {
            return walker;
        }
        match self {
            Expr::Skip => {}
            Expr::Ignore(expr) => {
                walker = expr.walk(walker);
            }
            Expr::Location(_, _) => {}
            Expr::Constant(_) => {}
            Expr::Unary(_, arg_expr) => {
                walker = arg_expr.walk(walker);
            }
            Expr::Binary(_, left_expr, right_expr) => {
                walker = left_expr.walk(walker);
                walker = right_expr.walk(walker);
            }
            Expr::Assign(left_expr, right_expr) => {
                walker = left_expr.walk(walker);
                walker = right_expr.walk(walker);
            }
            Expr::Seq(first_expr, second_expr) => {
                walker = first_expr.walk(walker);
                walker = second_expr.walk(walker);
            }
            Expr::Let(_, expr) => {
                walker = expr.walk(walker);
            }
            Expr::VarScope(_, val_expr, cont_expr) => {
                walker = val_expr.walk(walker);
                walker = cont_expr.walk(walker);
            }
            Expr::Function(_, func) => {
                let Function(_, body_expr) = func.deref();
                walker = body_expr.walk(walker);
            }
            Expr::FunScope(_, func, cont_expr) => {
                let Function(_, body_expr) = func.deref();
                walker = body_expr.walk(walker);
                walker = cont_expr.walk(walker);
            }
            Expr::If(cond_expr, true_expr, false_expr) => {
                walker = cond_expr.walk(walker);
                walker = true_expr.walk(walker);
                walker = false_expr.walk(walker);
            }
            Expr::Closure(func) => {
                let Function(_, body_expr) = func.deref();
                walker = body_expr.walk(walker);
            }
            Expr::Call(callee_expr, arg_exprs) => {
                walker = callee_expr.walk(walker);
                for arg_expr in arg_exprs.iter() {
                    walker = arg_expr.walk(walker)
                }
            }
            Expr::Return(expr) => {
                walker = expr.walk(walker);
            }
            Expr::While(cond_expr, inner_expr) => {
                walker = cond_expr.walk(walker);
                walker = inner_expr.walk(walker);
            }
            Expr::For(_, iter_expr, inner_expr) => {
                walker = iter_expr.walk(walker);
                walker = inner_expr.walk(walker);
            }
            Expr::Continue => {}
            Expr::Break => {}
        };
        walker
    }

    fn eval_impl(&self, env: &mut Environment) -> Flow {
        match self {
            Expr::Skip => Flow::Normal(Value::Void),
            Expr::Ignore(expr) => expr.eval_impl(env).map(|_| Value::Void),
            Expr::Location(ptr, _) => env.get(ptr.get()).unwrap_or(Value::Void).to_flow(),
            Expr::Constant(val) => val.clone().to_flow(),
            Expr::Unary(op, arg_expr) => {
                if *op == UnaryOp::Reference {
                    if let Expr::Location(ptr, _) = arg_expr.as_ref() {
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
                    UnaryOp::Reference => panic!("unreachable"),
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
            Expr::Let(_, _) => {
                panic!("Expr::Let is an intermediate node and can't be present in final AST")
            }
            Expr::VarScope(var, val_expr, cont_expr) => val_expr.eval_impl(env).and_then(|val| {
                env.stack.push(var.clone(), val);
                let res = cont_expr.eval_impl(env);
                env.stack.pop();
                res
            }),
            Expr::Function(_, _) => {
                panic!("Expr::Function is an intermediate node and can't be present in final AST")
            }
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
                                .for_each(|(name, val)| env.stack.push(name.clone(), val.clone()));
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
                            env.stack.push(var.clone(), Value::Number(i));
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

    pub fn eval(&self, mut env: Environment) -> Value {
        self.eval_impl(&mut env).to_value()
    }

    fn set_up_impl(&self, env: &mut Environment, sym_stack: &mut Vec<(String, Option<usize>)>) {
        let _ = self.walk(|expr| match expr {
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
                val_expr.set_up_impl(env, sym_stack);
                sym_stack.push((var.clone(), None));
                env.stack.push(var.clone(), Value::Void);
                cont_expr.set_up_impl(env, sym_stack);
                env.stack.pop();
                false
            }
            Expr::FunScope(name, func, cont_expr) => {
                // disallow accessing variables outside of closure
                let Function(arg_names, body_expr) = func.deref();
                sym_stack.push((name.clone(), Some(env.functions.len())));
                env.functions.push(func.clone());
                let mut new_stack = Stack::new();
                for arg_name in arg_names.iter() {
                    new_stack.push(arg_name.clone(), Value::Void);
                    sym_stack.push((arg_name.clone(), None));
                }
                mem::swap(&mut env.stack, &mut new_stack);
                body_expr.set_up_impl(env, sym_stack);
                mem::swap(&mut env.stack, &mut new_stack);
                for _ in arg_names.iter() {
                    sym_stack.pop();
                }
                cont_expr.set_up_impl(env, sym_stack);
                sym_stack.pop();
                false
            }
            Expr::Closure(func) => {
                // disallow accessing variables outside of closure
                let Function(arg_names, body_expr) = func.deref();
                let mut new_stack = Stack::new();
                for arg_name in arg_names.iter() {
                    new_stack.push(arg_name.clone(), Value::Void);
                    sym_stack.push((arg_name.clone(), None));
                }
                mem::swap(&mut env.stack, &mut new_stack);
                body_expr.set_up_impl(env, sym_stack);
                mem::swap(&mut env.stack, &mut new_stack);
                for _ in arg_names.iter() {
                    sym_stack.pop();
                }
                false
            }
            Expr::For(var, iter_expr, inner_expr) => {
                iter_expr.set_up_impl(env, sym_stack);
                sym_stack.push((var.clone(), None));
                env.stack.push(var.clone(), Value::Void);
                inner_expr.set_up_impl(env, sym_stack);
                env.stack.pop();
                false
            }
            _ => true,
        });
    }

    pub fn set_up(&self) -> Environment {
        let mut env = Environment::default();
        let mut sym_stack: Vec<(String, Option<usize>)> = Vec::new();
        self.set_up_impl(&mut env, &mut sym_stack);
        env
    }
}
