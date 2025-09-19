use core::ops::{Range, RangeInclusive};

use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use chumsky::span::SimpleSpan;
use hashbrown::HashMap;

use crate::{
    expr::Expr,
    parser::{Identifier, Variable},
    span::Spanned,
    typing::{Polytype, Type},
};

use nanorust_macros::ty;

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

    pub fn get_builtin_name(&self) -> &'static str {
        match self {
            UnaryOp::Negate => "__neg",
            UnaryOp::LogicNot => "__not",
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

    pub fn get_builtin_name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "__add",
            BinaryOp::Subtract => "__sub",
            BinaryOp::Multiply => "__mul",
            BinaryOp::Divide => "__div",
            BinaryOp::Modulo => "__rem",
            BinaryOp::Equal => "__eq",
            BinaryOp::NotEqual => "__ne",
            BinaryOp::LessEqual => "__le",
            BinaryOp::GreaterEqual => "__ge",
            BinaryOp::Less => "__lt",
            BinaryOp::Greater => "__gt",
            BinaryOp::LogicOr => "__or",
            BinaryOp::LogicAnd => "__and",
            BinaryOp::Range => todo!(),
            BinaryOp::RangeInclusive => todo!(),
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
    // pub fn to_absolute(self, stack: &EvalStack<'src>) -> Self {
    //     match self {
    //         Pointer::Relative(i) => Pointer::Absolute(stack.len() - i - 1),
    //         Pointer::Absolute(_) => self,
    //         Pointer::Function(_) => self,
    //         Pointer::Capture(i, _, var) => *stack
    //             .get_relative(i)
    //             .unwrap()
    //             .unwrap_closure_ref()
    //             .1
    //             .get(var)
    //             .unwrap(),
    //         Pointer::Invalid => unreachable!(),
    //     }
    // }

    pub fn to_absolute_sym(self, stack_len: usize) -> Self {
        match self {
            Pointer::Relative(i) => Pointer::Absolute(stack_len - i - 1),
            Pointer::Absolute(_) => self,
            Pointer::Function(_) => self,
            Pointer::Capture(_, i, _) => Pointer::Absolute(stack_len - i - 1),
            Pointer::Invalid => unreachable!(),
        }
    }

    pub fn unwrap_absolute(self) -> usize {
        self.try_unwrap_absolute()
            .unwrap_or_else(|| panic!("unwrap of non-absolute pointer {:?}", self))
    }

    pub fn try_unwrap_absolute(self) -> Option<usize> {
        match self {
            Pointer::Absolute(i) => Some(i),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Function<'src> {
    pub params: Vec<Identifier<'src>>,
    pub args: Vec<Identifier<'src>>,
    pub decl_span: SimpleSpan,
    pub body: Box<Spanned<Expr<'src>>>,
    pub ty: Polytype<'src>,
}

impl<'src> Function<'src> {
    pub fn new_function(
        type_params: Vec<Identifier<'src>>,
        vars: Vec<Variable<'src>>,
        ret_type: Type<'src>,
        decl_span: SimpleSpan,
        body: Spanned<Expr<'src>>,
    ) -> Self {
        let (var_names, var_types) = vars
            .into_iter()
            .map(|var| (var.name, var.ty))
            .unzip();
        let func_ty = Type::Function(var_types, Box::new(ret_type));
        assert!(
            !func_ty.has_unknowns(),
            "explicit types are required in function signatures"
        );
        Function {
            params: type_params.clone(),
            args: var_names,
            body: Box::new(body),
            decl_span: decl_span,
            ty: Polytype::from(type_params, func_ty),
        }
    }

    pub fn new_closure(
        vars: Vec<Variable<'src>>,
        ret_type: Type<'src>,
        decl_span: SimpleSpan,
        body: Spanned<Expr<'src>>,
    ) -> Self {
        let (var_names, var_types) = vars
            .into_iter()
            .map(|var| (var.name, var.ty))
            .unzip();
        Function {
            params: Vec::new(),
            args: var_names,
            body: Box::new(body),
            decl_span: decl_span,
            ty: Polytype::from_unknown(Type::Function(var_types, Box::new(ret_type))),
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
    pub fn unwrap_number(self) -> i32 {
        match self {
            Value::Number(val) => val,
            _ => panic!("unwrap of non-number value {:?}", self),
        }
    }

    pub fn unwrap_boolean(self) -> bool {
        match self {
            Value::Boolean(val) => val,
            _ => panic!("unwrap of non-boolean value {:?}", self),
        }
    }

    pub fn unwrap_range(self) -> Box<dyn Iterator<Item = i32>> {
        match self {
            Value::Range(range) => Box::new(range),
            Value::RangeInclusive(range) => Box::new(range),
            _ => panic!("unwrap of non-range value {:?}", self),
        }
    }

    // pub fn to_function(&self, env: &Environment<'src>) -> Rc<Function<'src>> {
    //     match self {
    //         Value::Function(i) => env.functions.get(*i).unwrap().clone(),
    //         Value::Closure(func, _) => func.clone(),
    //         _ => panic!("unwrap of non-functional value {:?}", self),
    //     }
    // }

    pub fn unwrap_closure_ref(&self) -> (Rc<Function<'src>>, &HashMap<String, Pointer<'src>>) {
        match &self {
            Value::Closure(func, captures) => (func.clone(), captures),
            _ => panic!("unwrap of non-closure value {:?}", self),
        }
    }

    // pub fn init_closure_captures(
    //     captures: &RefCell<HashMap<String, Pointer<'src>>>,
    //     stack: &EvalStack<'src>,
    // ) -> HashMap<String, Pointer<'src>> {
    //     let mut res = HashMap::new();
    //     captures.borrow().iter().for_each(|(var, ptr)| {
    //         res.insert(var.clone(), ptr.to_absolute(stack));
    //     });
    //     res
    // }
}
