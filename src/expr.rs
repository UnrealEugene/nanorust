use core::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    iter::zip,
    ops::{Deref, Range, RangeInclusive},
};

use alloc::{
    boxed::Box,
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use hashbrown::HashMap;

use crate::{
    span::Spanned,
    typing::{Polytype, Type},
};

use hex_coding_macros::ty;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Negate,
    LogicNot,
}

impl UnaryOp {
    pub fn calc<'src>(&self, arg: Value<'src>) -> Value<'src> {
        match self {
            UnaryOp::Negate => Value::Number(arg.unwrap_number().wrapping_neg()),
            UnaryOp::LogicNot => Value::Boolean(!arg.unwrap_boolean()),
        }
    }

    pub fn get_type(&self) -> Polytype<'static> {
        match self {
            UnaryOp::Negate => ty!("fn(i32) -> i32"),
            UnaryOp::LogicNot => ty!("fn(bool) -> bool"),
        }
    }
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
            BinaryOp::Add => Self::calc_num_to_num(a, b, i32::wrapping_add),
            BinaryOp::Subtract => Self::calc_num_to_num(a, b, i32::wrapping_sub),
            BinaryOp::Multiply => Self::calc_num_to_num(a, b, i32::wrapping_mul),
            BinaryOp::Divide => Self::calc_num_to_num(a, b, i32::wrapping_div),
            BinaryOp::Modulo => Self::calc_num_to_num(a, b, i32::wrapping_rem),
            BinaryOp::Equal => Self::calc_num_to_bool(a, b, i32::eq),
            BinaryOp::NotEqual => Self::calc_num_to_bool(a, b, i32::ne),
            BinaryOp::LessEqual => Self::calc_num_to_bool(a, b, i32::le),
            BinaryOp::GreaterEqual => Self::calc_num_to_bool(a, b, i32::ge),
            BinaryOp::Less => Self::calc_num_to_bool(a, b, i32::lt),
            BinaryOp::Greater => Self::calc_num_to_bool(a, b, i32::gt),
            BinaryOp::LogicOr => Self::calc_bool_to_bool(a, b, |a, b| a || b),
            BinaryOp::LogicAnd => Self::calc_bool_to_bool(a, b, |a, b| a && b),
            BinaryOp::Range => Self::calc_num_to_any(a, b, |a, b| a..b, Value::Range),
            BinaryOp::RangeInclusive => {
                Self::calc_num_to_any(a, b, |a, b| a..=b, Value::RangeInclusive)
            }
        }
    }

    pub fn get_type(&self) -> Polytype<'static> {
        match self {
            BinaryOp::Add => ty!("fn(i32, i32) -> i32"),
            BinaryOp::Subtract => ty!("fn(i32, i32) -> i32"),
            BinaryOp::Multiply => ty!("fn(i32, i32) -> i32"),
            BinaryOp::Divide => ty!("fn(i32, i32) -> i32"),
            BinaryOp::Modulo => ty!("fn(i32, i32) -> i32"),
            BinaryOp::Equal => ty!("fn('0, '0) -> bool"),
            BinaryOp::NotEqual => ty!("fn('0, '0) -> bool"),
            BinaryOp::LessEqual => ty!("fn(i32, i32) -> bool"),
            BinaryOp::GreaterEqual => ty!("fn(i32, i32) -> bool"),
            BinaryOp::Less => ty!("fn(i32, i32) -> bool"),
            BinaryOp::Greater => ty!("fn(i32, i32) -> bool"),
            BinaryOp::LogicOr => ty!("fn(bool, bool) -> bool"),
            BinaryOp::LogicAnd => ty!("fn(bool, bool) -> bool"),
            BinaryOp::Range => ty!("fn(i32, i32) -> range"),
            BinaryOp::RangeInclusive => ty!("fn(i32, i32) -> range"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Pointer<'src> {
    Relative(usize),
    Absolute(usize),
    Function(usize),
    Capture(usize, usize, &'src str),
    Invalid,
}

impl<'src> Pointer<'src> {
    pub fn to_absolute(self, stack: &Stack<'src>) -> Self {
        match self {
            Pointer::Relative(i) => Pointer::Absolute(stack.len() - i - 1),
            Pointer::Absolute(_) => self,
            Pointer::Function(_) => self,
            Pointer::Capture(i, _, var) => *stack
                .get_relative(i)
                .unwrap()
                .unwrap_closure_ref()
                .1
                .get(var)
                .unwrap(),
            Pointer::Invalid => unreachable!(),
        }
    }

    pub fn to_absolute_sym(self, stack_len: usize) -> Self {
        match self {
            Pointer::Relative(i) => Pointer::Absolute(stack_len - i - 1),
            Pointer::Absolute(_) => self,
            Pointer::Function(_) => self,
            Pointer::Capture(_, i, _) => Pointer::Absolute(stack_len - i - 1),
            Pointer::Invalid => unreachable!(),
        }
    }

    pub fn to_lvalue(self) -> CValue<'src> {
        CValue::LValue(self)
    }

    pub fn unwrap_absolute(self) -> usize {
        match self {
            Pointer::Absolute(i) => i,
            _ => panic!("unwrap of non-absolute pointer {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'src> {
    Number(i32),
    Boolean(bool),
    Tuple(Vec<Value<'src>>),
    Range(Range<i32>),
    RangeInclusive(RangeInclusive<i32>),
    Pointer(Pointer<'src>),
    Function(usize),
    Closure(Rc<Function<'src>>, HashMap<String, Pointer<'src>>),
}

impl<'src> Default for Value<'src> {
    fn default() -> Self {
        Value::Tuple(Vec::new())
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

    fn to_function(self, env: &Environment<'src>) -> Rc<Function<'src>> {
        match self {
            Value::Function(i) => env.functions.get(i).unwrap().clone(),
            Value::Closure(func, _) => func,
            _ => panic!("unwrap of non-functional value {:?}", self),
        }
    }

    fn unwrap_closure_ref(&self) -> (Rc<Function<'src>>, &HashMap<String, Pointer<'src>>) {
        match &self {
            Value::Closure(func, captures) => (func.clone(), captures),
            _ => panic!("unwrap of non-closure value {:?}", self),
        }
    }

    fn to_flow(self) -> Flow<'src> {
        Flow::Normal(CValue::RValue(self))
    }

    fn init_closure_captures(
        captures: &RefCell<HashMap<String, Pointer<'src>>>,
        stack: &Stack<'src>,
    ) -> HashMap<String, Pointer<'src>> {
        let mut res = HashMap::new();
        captures.borrow().iter().for_each(|(var, ptr)| {
            res.insert(var.clone(), ptr.to_absolute(stack));
        });
        res
    }
}

#[derive(Debug, Clone)]
pub enum CValue<'src> {
    LValue(Pointer<'src>),
    RValue(Value<'src>),
}

impl<'src> Default for CValue<'src> {
    fn default() -> Self {
        Self::RValue(Value::default())
    }
}

impl<'src> CValue<'src> {
    pub fn to_rvalue(self, env: &Environment<'src>) -> Self {
        match self {
            CValue::LValue(ptr) => CValue::RValue(env.stack.get(ptr).unwrap().clone()),
            CValue::RValue(_) => self,
        }
    }

    pub fn to_flow(self) -> Flow<'src> {
        Flow::Normal(self)
    }

    pub fn unwrap_lvalue(self) -> Pointer<'src> {
        match self {
            CValue::LValue(ptr) => ptr,
            CValue::RValue(_) => panic!("unwrap of non rvalue value"),
        }
    }

    pub fn unwrap_rvalue(self) -> Value<'src> {
        match self {
            CValue::LValue(_) => panic!("unwrap of non rvalue value"),
            CValue::RValue(val) => val,
        }
    }

    pub fn unwrap(self, env: &Environment<'src>) -> Value<'src> {
        self.to_rvalue(env).unwrap_rvalue()
    }
}

pub enum Flow<'src, R = CValue<'src>> {
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
    fn to_value(self) -> CValue<'src> {
        match self {
            Flow::Normal(val) => val,
            Flow::Return(val) => CValue::RValue(val),
            Flow::Continue => CValue::default(),
            Flow::Break => CValue::default(),
        }
    }
}

pub type Identifier<'src> = Spanned<&'src str>;

#[derive(Debug)]
pub struct Variable<'src, T = Type<'src>> {
    pub name: Identifier<'src>,
    pub ty: RefCell<T>,
}

#[derive(Debug)]
pub struct Function<'src> {
    pub args: Vec<Identifier<'src>>,
    pub body: Box<Spanned<Expr<'src>>>,
    pub ty: RefCell<Polytype<'src>>,
}

impl<'src> Function<'src> {
    pub fn new(vars: Vec<Variable<'src>>, ret_type: Type<'src>, body: Spanned<Expr<'src>>) -> Self {
        let (var_names, var_types) = vars
            .into_iter()
            .map(|var| (var.name, var.ty.into_inner()))
            .unzip();
        Function {
            args: var_names,
            body: Box::new(body),
            ty: RefCell::new(Polytype::from_unknown(Type::Function(
                var_types,
                Box::new(ret_type),
            ))),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'src> {
    Error,
    Skip,                                              // Void
    Block(Box<Spanned<Self>>),                         // any
    Ignore(Box<Spanned<Self>>),                        // Void
    Location(Cell<(Pointer<'src>, usize)>, &'src str), // Value
    Constant(Value<'src>),                             // Value
    Tuple(Vec<Spanned<Self>>),                         // Value
    Reference {
        is_mut: bool,
        expr: Box<Spanned<Self>>,
    }, // Value
    Dereference {
        is_mut: bool,
        expr: Box<Spanned<Self>>,
    }, // Value
    Unary(UnaryOp, Box<Spanned<Self>>),                // Value
    Binary(BinaryOp, Box<Spanned<Self>>, Box<Spanned<Self>>), // Value
    Assign(Box<Spanned<Self>>, Box<Spanned<Self>>),    // Void
    Seq(Box<Spanned<Self>>, Box<Spanned<Self>>),       // any
    Let {
        var: Variable<'src, Polytype<'src>>,
        is_mut: bool,
        val: Box<Spanned<Self>>,
    }, // Void
    VarScope {
        var: Variable<'src, Polytype<'src>>,
        is_mut: bool,
        val: Box<Spanned<Self>>,
        cont: Box<Spanned<Self>>,
    }, // any
    Function {
        name: Identifier<'src>,
        func: Rc<Function<'src>>,
    }, // Void
    FunScope {
        name: Identifier<'src>,
        func: Rc<Function<'src>>,
        cont: Box<Spanned<Self>>,
    }, // any
    Cast(Box<Spanned<Self>>, Polytype<'src>),          // Value
    If {
        cond: Box<Spanned<Self>>,
        if_false: Box<Spanned<Self>>,
        if_true: Box<Spanned<Self>>,
    }, // any
    Closure(Rc<Function<'src>>, RefCell<HashMap<String, Pointer<'src>>>), // Value
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),      // Value
    Return(Box<Spanned<Self>>),                        // Void
    While(Box<Spanned<Self>>, Box<Spanned<Self>>),     // Void
    For {
        var: Variable<'src>,
        iter: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    }, // Void
    Continue,                                          // Void
    Break,                                             // Void
}

impl<'src> Default for Expr<'src> {
    fn default() -> Self {
        Expr::Skip
    }
}

#[derive(Default)]
pub struct Stack<'src>(Vec<(&'src str, Value<'src>)>);

#[derive(Default)]
pub struct Environment<'src> {
    stack: Stack<'src>,
    functions: Vec<Rc<Function<'src>>>,
}

impl<'src> Stack<'src> {
    fn new() -> Self {
        Self::default()
    }

    fn get_relative(&self, i: usize) -> Option<&Value<'src>> {
        self.0.get(self.len() - i - 1).map(|e| &e.1)
    }

    fn get(&self, ptr: Pointer<'src>) -> Option<&Value<'src>> {
        let index = ptr.to_absolute(&self).unwrap_absolute();
        self.0.get(index).map(|e| &e.1)
    }

    fn index<Q>(&self, k: &Q) -> Option<usize>
    where
        &'src str: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0
            .iter()
            .rev()
            .position(|(var, _)| var.borrow() == k)
            .map(|i| self.0.len() - i - 1)
    }

    fn assign(&mut self, ptr: Pointer<'src>, v: Value<'src>) {
        let index = ptr.to_absolute(&self).unwrap_absolute();
        self.0.get_mut(index).map(|e| e.1 = v);
    }

    fn push(&mut self, k: &'src str, v: Value<'src>) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(&'src str, Value<'src>)> {
        self.0.pop()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'src> Environment<'src> {
    fn get(&self, ptr: Pointer<'src>) -> Option<Value<'src>> {
        self.stack.get(ptr).cloned().or_else(|| match ptr {
            Pointer::Function(i) => Some(Value::Function(i)),
            _ => None,
        })
    }
}

struct SymStack<'src>(Vec<Vec<(&'src str, Option<usize>, bool)>>);

impl<'src> SymStack<'src> {
    fn new() -> Self {
        SymStack(vec![Vec::new()])
    }

    fn push(&mut self, var: &'src str, func_ind: Option<usize>, is_mut: bool) {
        self.0.last_mut().unwrap().push((var, func_ind, is_mut))
    }

    fn push_closure(&mut self) {
        self.0.push(Vec::new());
    }

    fn pop(&mut self) {
        self.0.last_mut().unwrap().pop();
    }

    fn pop_closure(&mut self) {
        self.0.pop();
    }

    fn depth(&self) -> usize {
        self.0.len() - 1
    }

    fn closure_stack_size(&self, depth: usize) -> usize {
        self.0[depth].len()
    }

    fn find_var(&self, var: &'src str) -> Option<(usize, Option<usize>, bool, usize)> {
        let mut ind = 0;
        for (i, inner) in self.0.iter().enumerate().rev() {
            for (var2, func_ind, is_mut) in inner.iter().rev() {
                if var == *var2 {
                    return Some((ind, *func_ind, *is_mut, i));
                }
                ind += 1;
            }
        }
        None
    }

    fn index_at_depth(&self, var: &'src str, depth: usize) -> Option<usize> {
        self.0[depth]
            .iter()
            .rev()
            .position(|(var2, _, _)| var == *var2)
    }
}

impl<'src> Expr<'src> {
    pub fn new_ignore(inner_span: Spanned<Self>) -> Spanned<Self> {
        if inner_span.0.is_unit() {
            inner_span
        } else {
            let span = inner_span.span();
            Spanned(Expr::Ignore(Box::new(inner_span)), span)
        }
    }

    pub fn new_seq(first_span: Spanned<Self>, second_span: Spanned<Self>) -> Self {
        match first_span.0 {
            Expr::Let { var, is_mut, val } => Expr::VarScope {
                var,
                is_mut,
                val,
                cont: Box::new(second_span),
            },
            Expr::Function { name, func } => Expr::FunScope {
                name,
                func,
                cont: Box::new(second_span),
            },
            Expr::Skip => second_span.0,
            _ => Expr::Seq(
                Box::new(Self::new_ignore(first_span)),
                Box::new(second_span),
            ),
        }
    }

    /// Returns true if expression is guaranteed to have a unit type.
    fn is_unit(&self) -> bool {
        // TODO: maybe optimize
        match self {
            Expr::Error => true,
            Expr::Skip => true,
            Expr::Block(inner) => inner.0.is_unit(),
            Expr::Ignore(..) => true,
            Expr::Location(..) => false, // can't decide solely from AST
            Expr::Constant(..) => false,
            Expr::Tuple(..) => false,
            Expr::Reference { .. } => false,
            Expr::Dereference { .. } => false,
            Expr::Unary(..) => false,
            Expr::Binary(..) => false,
            Expr::Assign(..) => true,
            Expr::Seq(.., second) => second.0.is_unit(),
            Expr::Let { .. } => true,
            Expr::VarScope { cont, .. } => cont.0.is_unit(),
            Expr::Function { .. } => true,
            Expr::FunScope { cont, .. } => cont.0.is_unit(),
            Expr::Cast(..) => false,
            Expr::If { if_false, .. } => if_false.0.is_unit(),
            Expr::Closure(..) => false,
            Expr::Call(..) => false, // can't decide solely from AST
            Expr::Return(..) => true,
            Expr::While(..) => true,
            Expr::For { .. } => true,
            Expr::Continue => true,
            Expr::Break => true,
        }
    }

    pub fn walk<'a, F: FnMut(&'a Spanned<Self>) -> bool>(
        spanned_self: &'a Spanned<Self>,
        mut walker: F,
    ) -> F {
        let cont = walker(spanned_self);
        if !cont {
            return walker;
        }
        match &spanned_self.0 {
            Expr::Error => {}
            Expr::Skip => {}
            Expr::Block(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Ignore(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Location(..) => {}
            Expr::Constant(..) => {}
            Expr::Tuple(elems) => {
                for elem in elems.iter() {
                    walker = Self::walk(elem, walker)
                }
            }
            Expr::Reference { is_mut: _, expr } => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Dereference { is_mut: _, expr } => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::Unary(_, arg) => {
                walker = Self::walk(arg.as_ref(), walker);
            }
            Expr::Binary(_, left, right) => {
                walker = Self::walk(left.as_ref(), walker);
                walker = Self::walk(right.as_ref(), walker);
            }
            Expr::Assign(left, right) => {
                walker = Self::walk(left.as_ref(), walker);
                walker = Self::walk(right.as_ref(), walker);
            }
            Expr::Seq(first, second) => {
                walker = Self::walk(first.as_ref(), walker);
                walker = Self::walk(second.as_ref(), walker);
            }
            Expr::Let { val, .. } => {
                walker = Self::walk(val.as_ref(), walker);
            }
            Expr::VarScope { val, cont, .. } => {
                walker = Self::walk(val.as_ref(), walker);
                walker = Self::walk(cont.as_ref(), walker);
            }
            Expr::Function { func, .. } => {
                let Function { body, .. } = func.deref();
                walker = Self::walk(body.as_ref(), walker);
            }
            Expr::FunScope { func, cont, .. } => {
                let Function { body, .. } = func.deref();
                walker = Self::walk(body.as_ref(), walker);
                walker = Self::walk(cont.as_ref(), walker);
            }
            Expr::Cast(arg, _) => {
                walker = Self::walk(arg.as_ref(), walker);
            }
            Expr::If {
                cond,
                if_true,
                if_false,
            } => {
                walker = Self::walk(cond.as_ref(), walker);
                walker = Self::walk(if_true.as_ref(), walker);
                walker = Self::walk(if_false.as_ref(), walker);
            }
            Expr::Closure(func, _) => {
                let Function { body, .. } = func.deref();
                walker = Self::walk(body.as_ref(), walker);
            }
            Expr::Call(callee, args) => {
                walker = Self::walk(callee.as_ref(), walker);
                for arg in args.iter() {
                    walker = Self::walk(arg, walker)
                }
            }
            Expr::Return(expr) => {
                walker = Self::walk(expr.as_ref(), walker);
            }
            Expr::While(cond, inner) => {
                walker = Self::walk(cond.as_ref(), walker);
                walker = Self::walk(inner.as_ref(), walker);
            }
            Expr::For { iter, body, .. } => {
                walker = Self::walk(iter.as_ref(), walker);
                walker = Self::walk(body.as_ref(), walker);
            }
            Expr::Continue => {}
            Expr::Break => {}
        };
        walker
    }

    fn eval_impl(&self, env: &mut Environment<'src>) -> Flow<'src> {
        match self {
            Expr::Error => panic!("attempt to evaluate erroneous AST"),
            Expr::Skip => Flow::default(),
            Expr::Block(expr) => expr.0.eval_impl(env).map(|val| val.to_rvalue(env)),
            Expr::Ignore(expr) => expr.0.eval_impl(env).map(|_| CValue::default()),
            Expr::Location(ptr, _) => ptr.get().0.to_lvalue().to_flow(),
            Expr::Constant(val) => val.clone().to_flow(),
            Expr::Tuple(elems) => elems
                .iter()
                .fold(Flow::Normal(Vec::<Value<'src>>::new()), |vals, elem| {
                    vals.and_then(|mut vals| {
                        elem.0.eval_impl(env).map(|val| {
                            vals.push(val.to_rvalue(env).unwrap_rvalue());
                            vals
                        })
                    })
                })
                .map(Value::Tuple)
                .map(CValue::RValue),
            Expr::Reference { is_mut: _, expr } => {
                if let Expr::Location(ptr, _) = &expr.as_ref().0 {
                    return Value::Pointer(match ptr.get().0 {
                        Pointer::Function(i) => Pointer::Function(i),
                        ptr => ptr.to_absolute(&env.stack),
                    })
                    .to_flow();
                }
                unreachable!()
            }
            Expr::Dereference { is_mut: _, expr } => {
                expr.0.eval_impl(env).map(|arg| match arg.unwrap(env) {
                    Value::Pointer(ptr) => ptr.to_lvalue(),
                    _ => CValue::default(),
                })
            }
            Expr::Unary(op, arg) => arg
                .0
                .eval_impl(env)
                .map(|arg| CValue::RValue(op.calc(arg.unwrap(env)))),
            Expr::Binary(op, left, right) => left
                .0
                .eval_impl(env)
                .chain(|| right.0.eval_impl(env))
                .map(|(left, right)| op.calc(left.unwrap(env), right.unwrap(env)))
                .map(CValue::RValue),
            Expr::Assign(lhs, rhs) => {
                lhs.0
                    .eval_impl(env)
                    .chain(|| rhs.0.eval_impl(env))
                    .map(|(left, right)| {
                        env.stack.assign(left.unwrap_lvalue(), right.unwrap(env));
                        CValue::default()
                    })
            }
            Expr::Seq(first, second) => {
                first.0.eval_impl(env).and_then(|_| second.0.eval_impl(env))
            }
            Expr::Let { .. } => unreachable!(),
            Expr::VarScope {
                var,
                is_mut: _,
                val,
                cont,
            } => val.0.eval_impl(env).and_then(|val| {
                env.stack.push(var.name.0, val.unwrap(env));
                let res = cont.0.eval_impl(env).map(|v| v.to_rvalue(env));
                env.stack.pop();
                res
            }),
            Expr::Function { .. } => unreachable!(),
            Expr::FunScope { cont, .. } => cont.0.eval_impl(env),
            Expr::Cast(arg, _) => arg.0.eval_impl(env),
            Expr::If {
                cond,
                if_true,
                if_false,
            } => cond.0.eval_impl(env).and_then(|cond| {
                if cond.unwrap(env).unwrap_boolean() {
                    if_true.0.eval_impl(env)
                } else {
                    if_false.0.eval_impl(env)
                }
            }),
            Expr::Closure(func, captures) => Value::Closure(
                func.clone(),
                Value::init_closure_captures(captures, &env.stack),
            )
            .to_flow(),
            Expr::Call(callee_expr, args1) => callee_expr
                .0
                .eval_impl(env)
                .map(|val| val.unwrap(env))
                .and_then(|val: Value<'_>| {
                    // TODO: support functions too
                    let (func, _) = val.unwrap_closure_ref();
                    let Function { args, body, .. } = func.deref();
                    args1
                        .iter()
                        .fold(Flow::Normal(Vec::new()), |vals, arg| {
                            vals.and_then(|mut vals| {
                                arg.0.eval_impl(env).map(|val| {
                                    vals.push(val);
                                    vals
                                })
                            })
                        })
                        .map(|arg_vals| {
                            env.stack.push("", val);
                            zip(args.iter(), arg_vals.iter()).for_each(|(name, val)| {
                                env.stack.push(name.0, val.clone().unwrap(env))
                            });
                            let res = body.0.eval_impl(env).map(|val| val.to_rvalue(env));
                            args.iter().for_each(|_| {
                                env.stack.pop();
                            });
                            env.stack.pop();
                            res.to_value()
                        })
                }),
            Expr::Return(expr) => expr
                .0
                .eval_impl(env)
                .and_then(|val| Flow::Return(val.unwrap(env))),
            Expr::While(cond, inner) => loop {
                let flow = cond
                    .0
                    .eval_impl(env)
                    .and_then(|cond| {
                        if cond.unwrap(env).unwrap_boolean() {
                            Flow::default()
                        } else {
                            Flow::Break
                        }
                    })
                    .and_then(|_: ()| inner.0.eval_impl(env));
                match flow {
                    Flow::Return(_) => break flow,
                    Flow::Break => break Flow::default(),
                    _ => continue,
                }
            },
            Expr::For { var, iter, body } => iter.0.eval_impl(env).and_then(|val| {
                let flow = val.unwrap(env).unwrap_range().into_iter().fold(
                    Flow::default(),
                    |flow: Flow, i: i32| {
                        flow.and_then(|_| {
                            env.stack.push(var.name.0, Value::Number(i));
                            let res = match body.0.eval_impl(env) {
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
        self.eval_impl(&mut env).to_value().unwrap(&env)
    }

    fn set_up_impl<'a>(
        spanned_self: &'a Spanned<Self>,
        sym_stack: &mut SymStack<'src>,
        cap_stack: &mut Vec<&'a RefCell<HashMap<String, Pointer<'src>>>>,
    ) {
        let _ = Expr::walk(spanned_self, |expr| match &expr.0 {
            Expr::Location(cell, var) => {
                let (index, func_index, _, var_depth) = sym_stack
                    .find_var(var)
                    .unwrap_or_else(|| panic!("undefined variable `{}`", var));

                if let Some(i) = func_index {
                    cell.set((Pointer::Function(i), var_depth));
                    return true;
                };

                let depth = sym_stack.depth();
                let mut ptr = Pointer::Relative(sym_stack.index_at_depth(var, var_depth).unwrap());
                for d in var_depth..depth {
                    cap_stack[d]
                        .borrow_mut()
                        .entry(var.to_string())
                        .or_insert(ptr);
                    ptr = Pointer::Capture(sym_stack.closure_stack_size(d + 1), index, var);
                }
                cell.set((ptr, var_depth));
                true
            }
            Expr::Reference { is_mut, expr } => {
                Self::set_up_impl(expr.as_ref(), sym_stack, cap_stack);
                let Expr::Location(_, var) = &expr.0 else {
                    panic!("getting address of arbitrary expression is not supported");
                };
                let var_is_mut = sym_stack.find_var(var).map(|x| x.2).unwrap();
                if *is_mut && !var_is_mut {
                    panic!("cannot mutate immutable variable {}", var)
                }
                false
            }
            Expr::Assign(lhs, _) => {
                match &lhs.as_ref().0 {
                    Expr::Location(_, var) => {
                        if !sym_stack.find_var(var).map(|x| x.2).unwrap() {
                            panic!("cannot mutate immutable variable {}", var)
                        }
                    }
                    _ => {}
                };
                true
            }
            Expr::VarScope {
                var,
                is_mut,
                val,
                cont,
            } => {
                Self::set_up_impl(val.as_ref(), sym_stack, cap_stack);
                sym_stack.push(var.name.0, None, *is_mut);
                Self::set_up_impl(cont.as_ref(), sym_stack, cap_stack);
                sym_stack.pop();
                false
            }
            Expr::FunScope { name, func, cont } => {
                // disallow accessing variables outside of closure
                // let Function { args, body, .. } = func.deref();
                // sym_stack.push(name.0, Some(env.functions.len()), false);
                // env.functions.push(func.clone());
                // let mut new_stack = Stack::new();
                // for name in args.iter() {
                //     new_stack.push(name.0, Value::default());
                //     sym_stack.push(name.0, None, false);
                // }
                // mem::swap(&mut env.stack, &mut new_stack);
                // Self::set_up_impl(body.as_ref(), env, sym_stack);
                // mem::swap(&mut env.stack, &mut new_stack);
                // for _ in args.iter() {
                //     sym_stack.pop();
                // }
                // Self::set_up_impl(cont.as_ref(), env, sym_stack);
                // sym_stack.pop();
                false
            }
            Expr::Closure(func, captures) => {
                let Function { args, body, .. } = func.deref();
                sym_stack.push_closure();
                for name in args.iter() {
                    sym_stack.push(name.0, None, false);
                }
                cap_stack.push(captures);
                Self::set_up_impl(body.as_ref(), sym_stack, cap_stack);
                sym_stack.pop_closure();
                cap_stack.pop();
                false
            }
            Expr::For {
                var: Variable { name, .. },
                iter,
                body,
            } => {
                Self::set_up_impl(iter.as_ref(), sym_stack, cap_stack);
                sym_stack.push(name.0, None, false);
                Self::set_up_impl(body.as_ref(), sym_stack, cap_stack);
                sym_stack.pop();
                false
            }
            _ => true,
        });
    }

    pub fn set_up<'a>(spanned_self: &'a Spanned<Self>) -> Environment<'src> {
        let mut sym_stack: SymStack<'src> = SymStack::new();
        let mut cap_stack: Vec<&'a RefCell<HashMap<String, Pointer<'src>>>> = Vec::new();
        Self::set_up_impl(spanned_self, &mut sym_stack, &mut cap_stack);
        Environment::default()
    }
}
