use core::cell::RefCell;

use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use hashbrown::HashMap;

use crate::{
    parser::{Identifier, Keyword, Variable},
    span::Spanned,
    typing::{Polytype, Type},
    value::{BinaryOp, Function, Pointer, UnaryOp, Value},
};

#[derive(Debug, Default)]
pub enum Expr<'src> {
    Error,
    #[default]
    Skip, // Void
    Block(Box<Spanned<Self>>), // any
    Location {
        name: Identifier<'src>,
        bindings: Vec<Type<'src>>,
    }, // Value
    Constant(Value<'src>),     // Value
    Tuple(Vec<Spanned<Self>>), // Value
    Reference {
        is_mut: bool,
        expr: Box<Spanned<Self>>,
    }, // Value
    Dereference {
        is_mut: bool,
        expr: Box<Spanned<Self>>,
    }, // Value
    Unary(Spanned<UnaryOp>, Box<Spanned<Self>>), // Value
    Binary(Spanned<BinaryOp>, Box<Spanned<Self>>, Box<Spanned<Self>>), // Value
    Assign(Box<Spanned<Self>>, Box<Spanned<Self>>), // Void
    Seq(Box<Spanned<Self>>, Box<Spanned<Self>>), // any
    Let {
        var: Variable<'src, Polytype<'src>>,
        is_mut: bool,
        val: Box<Spanned<Self>>,
    }, // Void
    Function {
        name: Identifier<'src>,
        func: Function<'src>,
    }, // Void
    Cast(Box<Spanned<Self>>, Polytype<'src>), // Value
    If {
        cond: Box<Spanned<Self>>,
        if_true: Box<Spanned<Self>>,
        if_false: Option<Box<Spanned<Self>>>,
    }, // any
    Closure(Rc<Function<'src>>, RefCell<HashMap<String, Pointer<'src>>>), // Value
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>), // Value
    Return(Box<Spanned<Self>>), // Void
    While {
        cond: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    }, // Void
    For {
        var: Variable<'src>,
        iter: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    }, // Void
    Continue(Keyword<'src>),   // Void
    Break(Keyword<'src>),      // Void
}
