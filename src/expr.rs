use core::{
    cell::{Cell, RefCell},
    ops::Deref,
};

use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
use hashbrown::HashMap;

use crate::{
    parser::{Identifier, Variable},
    span::Spanned,
    typing::Polytype,
    value::{BinaryOp, Function, Pointer, UnaryOp, Value},
};

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
}
