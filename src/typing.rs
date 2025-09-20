// Algorithm W implementation of type inference in Hindley-Milner type system.
// Based on implementation from https://github.com/nwoeanhinnogaehr/algorithmw-rust

use core::fmt::{self, Display};
use core::hash::Hash;
use core::result;

use alloc::{boxed::Box, vec::Vec};

use chumsky::span::SimpleSpan;
use hashbrown::{HashMap, HashSet};

use crate::error::*;
use crate::ir::{FuncInfo, IR, Node, RValue, RValueInner, Reference, Tree, Value};
use crate::parser::Identifier;
use crate::span::Spanned;

pub type Result<'src, T> = result::Result<T, Box<dyn SemanticError + 'src>>;

trait HMType<'src>: Sized {
    fn get_free_vars(&self) -> HashSet<TypeVar>;
    fn substitute(&self, subst: &Subst<'src>) -> Self;
}

impl<'src, T: HMType<'src>> HMType<'src> for Vec<T> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(T::get_free_vars)
            .fold(HashSet::new(), |acc, t| acc.union(&t).cloned().collect())
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        self.iter().map(|x| x.substitute(subst)).collect()
    }
}

impl<'src, T: HMType<'src>> HMType<'src> for Vec<(Identifier<'src>, T)> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(|(_, x)| x)
            .map(T::get_free_vars)
            .fold(HashSet::new(), |acc, t| acc.union(&t).cloned().collect())
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        self.iter()
            .map(|(ident, x)| (*ident, x.substitute(subst)))
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    I32,
    Bool,
    Range,
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BuiltinType::I32 => "i32",
                BuiltinType::Bool => "bool",
                BuiltinType::Range => "range",
            }
        )
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub usize);

impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "T{}", self.0)
    }
}

impl TypeVar {
    fn bind<'src>(&self, ty: &Type<'src>) -> UnifyResult<'src> {
        if let Type::Variable(u) = ty {
            if u == self {
                return Ok(Subst::new());
            }
        }
        if ty.get_free_vars().contains(self) {
            return Err(UnifyError::OccursCheck);
        }
        let mut s = Subst::new();
        s.0.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

#[derive(Debug, Clone)]
struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    fn new() -> Self {
        TypeVarGen { supply: 0 }
    }

    fn new_from_offset(offset: usize) -> Self {
        TypeVarGen { supply: offset }
    }

    fn next(&mut self) -> TypeVar {
        let res = self.supply;
        self.supply += 1;
        TypeVar(res)
    }
}

enum UnifyError {
    OccursCheck,
    TypeMismatch,
    IllegalCast,
    LessGeneralType,
}

impl UnifyError {
    fn to_semantic_error<'src>(
        self,
        span: SimpleSpan,
        ty1: &Type<'src>,
        ty2: &Type<'src>,
    ) -> Box<dyn SemanticError + 'src> {
        match self {
            UnifyError::OccursCheck => Box::new(RecursiveTypeError {
                span,
                first_type: ty1.clone(),
                second_type: ty2.clone(),
            }),
            UnifyError::TypeMismatch => Box::new(TypeMismatchError {
                span,
                expected_type: ty1.clone(),
                actual_type: ty2.clone(),
            }),
            UnifyError::IllegalCast => todo!(),
            UnifyError::LessGeneralType => todo!(),
        }
    }
}

type UnifyResult<'src> = result::Result<Subst<'src>, UnifyError>;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Type<'src> {
    Builtin(BuiltinType),
    Concrete(Identifier<'src>),
    Variable(TypeVar),
    Reference {
        mutable: bool,
        inner: Box<Self>,
    },
    Tuple(Vec<Self>),
    Function(Vec<Self>, Box<Self>),
    LValue {
        mutable: bool,
        def_ident: Identifier<'src>,
        inner: Box<Self>,
    },
    #[default]
    Unknown,
}

fn write_vec<T: Display>(f: &mut fmt::Formatter<'_>, elems: &Vec<T>) -> fmt::Result {
    let mut first = true;
    for elem in elems.iter() {
        if !first {
            write!(f, ", ")?
        }
        write!(f, "{}", elem)?;
        first = false;
    }
    Ok(())
}

impl<'src> Display for Type<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Builtin(t) => t.fmt(f),
            Type::Concrete(s) => s.0.fmt(f),
            Type::Variable(var) => var.fmt(f),
            Type::Reference {
                mutable: is_mut,
                inner: ty,
            } => {
                if *is_mut {
                    write!(f, "&mut {}", ty)
                } else {
                    write!(f, "&{}", ty)
                }
            }
            Type::Tuple(elems) => {
                write!(f, "(")?;
                write_vec(f, elems)?;
                if elems.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Type::Function(args, res) => {
                write!(f, "fn(")?;
                write_vec(f, args)?;
                write!(f, ") -> {}", res)
            }
            Type::LValue {
                mutable: _,
                def_ident: _,
                inner: ty,
            } => ty.fmt(f),
            Type::Unknown => write!(f, "?"),
        }
    }
}

impl<'src> HMType<'src> for Type<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Builtin(..) => HashSet::new(),
            Type::Concrete(..) => HashSet::new(),
            Type::Variable(i) => [*i].into_iter().collect(),
            Type::Reference { inner: ty, .. } => ty.get_free_vars(),
            Type::Tuple(ts) => ts.get_free_vars(),
            Type::Function(args, ret) => args
                .get_free_vars()
                .union(&ret.get_free_vars())
                .cloned()
                .collect(),
            Type::LValue {
                mutable: _,
                def_ident: _,
                inner: ty,
            } => ty.get_free_vars(),
            Type::Unknown => unreachable!(),
        }
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        match self {
            Type::Builtin(..) => self.clone(),
            Type::Concrete(..) => self.clone(),
            Type::Variable(i) => subst.0.get(i).unwrap_or(self).clone(),
            Type::Reference {
                mutable: is_mut,
                inner: ty,
                ..
            } => Type::Reference {
                mutable: *is_mut,
                inner: Box::new(ty.substitute(subst)),
            },
            Type::Tuple(ts) => Type::Tuple(ts.substitute(subst)),
            Type::Function(args, ret) => {
                Type::Function(args.substitute(subst), Box::new(ret.substitute(subst)))
            }
            Type::LValue {
                mutable,
                def_ident,
                inner: ty,
            } => Type::LValue {
                mutable: *mutable,
                def_ident: *def_ident,
                inner: Box::new(ty.substitute(subst)),
            },
            Type::Unknown => unreachable!(),
        }
    }
}

#[allow(dead_code)]
impl<'src> Type<'src> {
    pub fn has_unknowns(&self) -> bool {
        match self {
            Type::Reference { inner: ty, .. } => ty.has_unknowns(),
            Type::Tuple(elems) => elems
                .iter()
                .map(|ty| ty.has_unknowns())
                .reduce(|x, y| x || y)
                .unwrap_or(false),
            Type::Function(args, ret) => {
                args.iter()
                    .map(|ty| ty.has_unknowns())
                    .reduce(|x, y| x || y)
                    .unwrap_or(false)
                    || ret.has_unknowns()
            }
            Type::LValue { inner: ty, .. } => ty.has_unknowns(),
            Type::Unknown => true,
            _ => false,
        }
    }

    fn replace_unknowns(self, tvg: &mut TypeVarGen) -> Self {
        match self {
            Type::Builtin(_) => self,
            Type::Concrete(_) => self,
            Type::Variable(_) => unreachable!(),
            Type::Reference {
                mutable: is_mut,
                inner: ty,
            } => Type::Reference {
                mutable: is_mut,
                inner: Box::new(ty.replace_unknowns(tvg)),
            },
            Type::Tuple(elems) => {
                Type::Tuple(elems.into_iter().map(|e| e.replace_unknowns(tvg)).collect())
            }
            Type::Function(args, ret) => Type::Function(
                args.into_iter().map(|e| e.replace_unknowns(tvg)).collect(),
                Box::new(ret.replace_unknowns(tvg)),
            ),
            Type::LValue {
                mutable,
                def_ident,
                inner: ty,
            } => Type::LValue {
                mutable,
                def_ident,
                inner: Box::new(ty.replace_unknowns(tvg)),
            },
            Type::Unknown => Type::Variable(tvg.next()),
        }
    }

    fn instantiate(self, type_vars: &Vec<Identifier<'src>>, offset: usize) -> Self {
        match self {
            Type::Builtin(_) => self,
            Type::Concrete(name) => {
                if let Some(i) = type_vars.iter().rev().position(|p| p.0 == name.0) {
                    Type::Variable(TypeVar(offset + type_vars.len() - i - 1))
                } else {
                    Type::Concrete(name)
                }
            }
            Type::Variable(_) => self,
            Type::Reference {
                mutable: is_mut,
                inner: ty,
            } => Type::Reference {
                mutable: is_mut,
                inner: Box::new(ty.instantiate(type_vars, offset)),
            },
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .into_iter()
                    .map(|e| e.instantiate(type_vars, offset))
                    .collect(),
            ),
            Type::Function(args, ret) => Type::Function(
                args.into_iter()
                    .map(|e| e.instantiate(type_vars, offset))
                    .collect(),
                Box::new(ret.instantiate(type_vars, offset)),
            ),
            Type::LValue {
                mutable,
                def_ident,
                inner: ty,
            } => Type::LValue {
                mutable,
                def_ident,
                inner: Box::new(ty.instantiate(type_vars, offset)),
            },
            Type::Unknown => self,
        }
    }

    fn next_free_var(&self) -> usize {
        match self {
            Type::Builtin(_) => 0,
            Type::Concrete(_) => 0,
            Type::Variable(TypeVar(i)) => i + 1,
            Type::Reference { inner: ty, .. } => ty.next_free_var(),
            Type::Tuple(elems) => elems
                .iter()
                .map(Self::next_free_var)
                .reduce(usize::max)
                .unwrap_or(0),
            Type::Function(args, ret) => args
                .iter()
                .map(Self::next_free_var)
                .reduce(usize::max)
                .unwrap_or(0)
                .max(ret.next_free_var()),
            Type::LValue { inner: ty, .. } => ty.next_free_var(),
            Type::Unknown => 0,
        }
    }

    pub fn unit() -> Self {
        Type::Tuple(Vec::new())
    }

    fn never(tvg: &mut TypeVarGen) -> Self {
        Type::Variable(tvg.next())
    }

    fn unify_vec(first: &Vec<Self>, second: &Vec<Self>) -> UnifyResult<'src> {
        assert!(first.len() == second.len());
        first
            .iter()
            .zip(second.iter())
            .fold(Ok(Subst::new()), |res, (t1, t2)| {
                res.and_then(|subst1| {
                    t1.substitute(&subst1)
                        .unify(&t2.substitute(&subst1))
                        .map(|subst2| subst1.compose(&subst2))
                })
            })
    }

    fn unify(&self, other: &Self) -> UnifyResult<'src> {
        match (self, other) {
            (
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty1,
                },
                ty2,
            ) => ty1.unify(ty2),
            (
                ty1,
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty2,
                },
            ) => ty1.unify(ty2),
            (Type::Builtin(t1), Type::Builtin(t2)) if t1 == t2 => Ok(Subst::new()),
            (Type::Concrete(t1), Type::Concrete(t2)) if t1.0 == t2.0 => Ok(Subst::new()),
            (Type::Variable(var), t) => var.bind(t),
            (
                Type::Reference {
                    mutable: is_mut1,
                    inner: ty1,
                },
                Type::Reference {
                    mutable: is_mut2,
                    inner: ty2,
                },
            ) if is_mut1 == is_mut2 => ty1.unify(ty2),
            (t, Type::Variable(var)) => var.bind(t),
            (Type::Tuple(ts1), Type::Tuple(ts2)) if ts1.len() == ts2.len() => {
                Self::unify_vec(ts1, ts2)
            }
            (Type::Function(args1, ret1), Type::Function(args2, ret2))
                if args1.len() == args2.len() =>
            {
                let subst1 = Self::unify_vec(args1, args2)?;
                let subst2 = ret1.substitute(&subst1).unify(&ret2.substitute(&subst1))?;
                Ok(subst1.compose(&subst2))
            }
            (_, _) => Err(UnifyError::TypeMismatch),
        }
    }

    fn cast_unify(&self, other: &Self) -> UnifyResult<'src> {
        match (self, other) {
            (Type::Builtin(..), Type::Builtin(..)) => Ok(Subst::new()),
            (
                Type::Reference {
                    mutable: is_mut1,
                    inner: ty1,
                },
                Type::Reference {
                    mutable: is_mut2,
                    inner: ty2,
                },
            ) if !is_mut2 || *is_mut1 => ty1.unify(ty2),
            (
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty1,
                },
                ty2,
            ) => ty1.cast_unify(ty2),
            (
                ty1,
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty2,
                },
            ) => ty1.cast_unify(ty2),
            (_, _) => Err(UnifyError::IllegalCast),
        }
    }

    fn is_generalization_of_vec(
        left: &Vec<Type<'src>>,
        right: &Vec<Type<'src>>,
    ) -> UnifyResult<'src> {
        left.iter()
            .zip(right.iter())
            .fold(Ok(Subst::new()), |res, (t1, t2)| {
                res.and_then(|subst1| {
                    t1.is_generalization_of(t2)
                        .and_then(|subst2| subst1.strict_compose(&subst2))
                })
            })
    }

    fn is_generalization_of(&self, other: &Type<'src>) -> UnifyResult<'src> {
        match (self, other) {
            (
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty1,
                },
                ty2,
            ) => ty1.is_generalization_of(ty2),
            (
                ty1,
                Type::LValue {
                    mutable: _,
                    def_ident: _,
                    inner: ty2,
                },
            ) => ty1.is_generalization_of(ty2),
            (Type::Builtin(t1), Type::Builtin(t2)) if t1 == t2 => Ok(Subst::new()),
            (Type::Concrete(t1), Type::Concrete(t2)) if t1.0 == t2.0 => Ok(Subst::new()),
            (Type::Variable(var), t) => var.bind(t),
            (_, Type::Variable(_)) => Err(UnifyError::LessGeneralType),
            (
                Type::Reference {
                    mutable: is_mut1,
                    inner: ty1,
                },
                Type::Reference {
                    mutable: is_mut2,
                    inner: ty2,
                },
            ) if is_mut1 == is_mut2 => ty1.is_generalization_of(ty2),
            (Type::Tuple(ts1), Type::Tuple(ts2)) if ts1.len() == ts2.len() => {
                Self::is_generalization_of_vec(ts1, ts2)
            }
            (Type::Function(args1, ret1), Type::Function(args2, ret2))
                if args1.len() == args2.len() =>
            {
                let subst1 = Self::is_generalization_of_vec(args1, args2)?;
                let subst2 = ret1.is_generalization_of(ret2)?;
                subst1.strict_compose(&subst2)
            }
            (_, _) => Err(UnifyError::TypeMismatch),
        }
    }

    fn unwrap_function_ref(&self) -> (&Vec<Type<'src>>, &Type<'src>) {
        match self {
            Type::Function(args_ty, ret_ty) => (args_ty, ret_ty.as_ref()),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Polytype<'src> {
    vars: Vec<TypeVar>,
    ty: Type<'src>,
}

impl<'src> Display for Polytype<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            self.ty.fmt(f)
        } else {
            write!(f, "for<")?;
            write_vec(f, &self.vars)?;
            write!(f, "> {}", self.ty)
        }
    }
}

impl<'src> HMType<'src> for Polytype<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        self.ty
            .get_free_vars()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        Self {
            vars: self.vars.clone(),
            ty: self.ty.substitute(&subst.difference(self.vars.iter())),
        }
    }
}

impl<'src> Into<Type<'src>> for Polytype<'src> {
    fn into(self) -> Type<'src> {
        self.ty
    }
}

impl<'src> Polytype<'src> {
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Type<'src> {
        self.ty.substitute(&Subst(
            self.vars
                .iter()
                .cloned()
                .zip(self.vars.iter().map(|_| Type::Variable(tvg.next())))
                .collect(),
        ))
    }

    pub fn new_generalized(ty: Type<'src>) -> Self {
        Polytype {
            vars: ty.get_free_vars().iter().cloned().collect(),
            ty,
        }
    }

    pub fn from(type_vars: Vec<Identifier<'src>>, ty: Type<'src>) -> Self {
        let ty_bound = ty.instantiate(&type_vars, 0);
        Polytype {
            vars: (0..type_vars.len()).into_iter().map(TypeVar).collect(),
            ty: ty_bound,
        }
    }

    pub fn from_unknown(ty: Type<'src>) -> Self {
        let mut tvg = TypeVarGen::new();
        let new_ty = ty.replace_unknowns(&mut tvg);
        Polytype {
            vars: new_ty.get_free_vars().iter().cloned().collect(),
            ty: new_ty,
        }
    }

    pub fn bind_params(&self, bindings: &Vec<Type<'src>>) -> Self {
        let offset = self.ty.next_free_var();
        let mut tvg = TypeVarGen::new_from_offset(offset);
        let subst = Subst(
            self.vars
                .iter()
                .cloned()
                .zip(
                    bindings
                        .iter()
                        .cloned()
                        .map(|ty| ty.replace_unknowns(&mut tvg)),
                )
                .collect(),
        );
        Self::new_generalized(self.ty.substitute(&subst))
    }

    pub fn get_bind_arity(&self) -> usize {
        self.vars.len()
    }
}

#[derive(Debug, Clone)]
pub enum LetType<'src> {
    Poly(Polytype<'src>),
    Mono { mutable: bool, inner: Type<'src> },
}

impl<'src> HMType<'src> for LetType<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        match self {
            LetType::Poly(inner) => inner.get_free_vars(),
            LetType::Mono { inner, .. } => inner.get_free_vars(),
        }
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        match self {
            LetType::Poly(inner) => LetType::Poly(inner.substitute(subst)),
            LetType::Mono { mutable, inner } => LetType::Mono {
                mutable: *mutable,
                inner: inner.substitute(subst),
            },
        }
    }
}

#[derive(Debug, Clone)]
struct Subst<'src>(HashMap<TypeVar, Type<'src>>);

impl<'src> Subst<'src> {
    fn new() -> Self {
        Subst(HashMap::new())
    }

    fn compose(&self, other: &Self) -> Self {
        let new_other = other
            .0
            .iter()
            .map(|(k, v)| (k, v.substitute(self)))
            .collect::<Vec<_>>();
        let mut new_self = self.clone();
        for (k, v) in new_other.into_iter() {
            new_self.0.insert(k.clone(), v);
        }
        new_self
    }

    fn strict_compose(&self, other: &Self) -> UnifyResult<'src> {
        let mut new_self = self.clone();
        for (var, ty2) in other.0.iter() {
            if !self.0.contains_key(var) {
                new_self.0.insert(*var, ty2.clone());
            } else {
                let ty1 = self.0.get(var).unwrap();
                if ty1 != ty2 {
                    return Err(UnifyError::TypeMismatch);
                }
            }
        }
        Ok(new_self)
    }

    fn difference<'a, I>(&self, vars: I) -> Self
    where
        I: Iterator<Item = &'a TypeVar>,
    {
        let mut new_self = self.clone();
        for var in vars {
            new_self.0.remove(var);
        }
        new_self
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'src> {
    symbol_stack: Vec<(Identifier<'src>, LetType<'src>)>,
    func_type_list: Vec<Polytype<'src>>,
    func_ret_stack: Vec<Type<'src>>,
}

impl<'src> HMType<'src> for TypeEnv<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        self.symbol_stack.clone().get_free_vars()
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        TypeEnv {
            symbol_stack: self
                .symbol_stack
                .iter()
                .map(|(ident, t)| (*ident, t.substitute(subst)))
                .collect(),
            func_type_list: self.func_type_list.clone(),
            func_ret_stack: self
                .func_ret_stack
                .iter()
                .map(|t| t.substitute(subst))
                .collect(),
        }
    }
}

trait TupleChain<'src>: Sized + Clone {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, f: F) -> Self;
}

impl<'src> TupleChain<'src> for () {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, _: F) -> Self {
        self
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Type<'src>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, mut f: F) -> Self {
        (self.0.map(&mut f), f(self.1))
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Spanned<Type<'src>>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, mut f: F) -> Self {
        (self.0.map(&mut f), self.1.map(f))
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Polytype<'src>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, f: F) -> Self {
        (self.0.map(f), self.1)
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Spanned<Polytype<'src>>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, f: F) -> Self {
        (self.0.map(f), self.1)
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Vec<Type<'src>>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, mut f: F) -> Self {
        (self.0.map(&mut f), self.1.into_iter().map(f).collect())
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Vec<Spanned<Type<'src>>>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, mut f: F) -> Self {
        (self.0.map(&mut f), {
            let mut vec = Vec::new();
            for ty in self.1.into_iter() {
                vec.push(Spanned(f(ty.0), ty.1));
            }
            vec
        })
    }
}

struct TypeEnvMonadState<'g, 'src, I> {
    tvg: &'g mut TypeVarGen,
    env: &'g mut TypeEnv<'src>,
    subst: Subst<'src>,
    val: I,
}

impl<'g, 'src, I> TypeEnvMonadState<'g, 'src, I> {
    fn map_val<R, F: FnOnce(I) -> R>(self, f: F) -> TypeEnvMonadState<'g, 'src, R> {
        TypeEnvMonadState {
            tvg: self.tvg,
            env: self.env,
            subst: self.subst,
            val: f(self.val),
        }
    }
}

impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonadState<'g, 'src, I> {
    fn substitute(self, subst: &Subst<'src>) -> Self {
        *self.env = self.env.substitute(&subst);
        TypeEnvMonadState {
            tvg: self.tvg,
            env: self.env,
            subst: subst.compose(&self.subst),
            val: self.val.map(|t| t.substitute(subst)),
        }
    }
}

struct TypeEnvMonad<'g, 'src, I>(Result<'src, TypeEnvMonadState<'g, 'src, I>>);

#[allow(dead_code)]
impl<'g, 'src> TypeEnvMonad<'g, 'src, Type<'src>> {
    fn return_last(self) -> Result<'src, (Subst<'src>, Type<'src>)> {
        self.0.map(|st| {
            let new_val = st.val.substitute(&st.subst);
            (st.subst, new_val)
        })
    }
}

#[allow(dead_code)]
impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
    fn generalized(self) -> TypeEnvMonad<'g, 'src, (I, Polytype<'src>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = st.env.generalize(&st.val.1);
            st.map_val(|(l, _)| (l, new_ty))
        }))
    }

    fn unified_with(self, span: SimpleSpan, ty: Type<'src>) -> Self {
        self.substitute(|val| {
            val.1
                .unify(&ty)
                .map_err(|err| err.to_semantic_error(span, &ty, &val.1))
        })
    }

    fn spanned(self, span: SimpleSpan) -> TypeEnvMonad<'g, 'src, (I, Spanned<Type<'src>>)> {
        TypeEnvMonad(self.0.map(|st| st.map_val(|(l, r)| (l, Spanned(r, span)))))
    }
}

#[allow(dead_code)]
impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, (I, Spanned<Type<'src>>)> {
    fn generalized(self) -> TypeEnvMonad<'g, 'src, (I, Spanned<Polytype<'src>>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = Spanned(st.env.generalize(&st.val.1.0), st.val.1.1);
            st.map_val(|(l, _)| (l, new_ty))
        }))
    }

    fn unified_with(self, ty: Type<'src>) -> Self {
        self.substitute(|val| {
            val.1
                .0
                .unify(&ty)
                .map_err(|err| err.to_semantic_error(val.1.1, &ty, &val.1.0))
        })
    }

    fn return_last(self) -> Result<'src, (Subst<'src>, Spanned<Type<'src>>)> {
        self.0.map(|st| {
            let new_val = st.val.1.map(|ty| ty.substitute(&st.subst));
            (st.subst, new_val)
        })
    }
}

#[allow(dead_code)]
impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, (I, Polytype<'src>)> {
    fn instantiated(self) -> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = st.val.1.instantiate(st.tvg);
            st.map_val(|(l, _)| (l, new_ty))
        }))
    }
}

#[allow(dead_code)]
impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, I> {
    fn infer_type(self, node: &mut Node<'src>) -> TypeEnvMonad<'g, 'src, (I, Spanned<Type<'src>>)> {
        TypeEnvMonad(self.0.and_then(|st| {
            let (subst, ty) = st.env.infer_type_impl(node, st.tvg)?;
            Ok(st.substitute(&subst).map_val(|st_val| (st_val, ty)))
        }))
    }

    fn infer_types(
        self,
        exprs: &mut Vec<Node<'src>>,
    ) -> TypeEnvMonad<'g, 'src, (I, Vec<Spanned<Type<'src>>>)> {
        TypeEnvMonad(
            exprs
                .iter_mut()
                .fold(self.0.map(|st| (st, Vec::new())), |st, node| {
                    let (st, mut vec) = st?;
                    let (subst, ty) = st.env.infer_type_impl(node, st.tvg)?;
                    vec.push(ty);
                    Ok((st.substitute(&subst), vec))
                })
                .map(|(st, vec)| st.map_val(|st_val| (st_val, vec))),
        )
    }

    fn process_function(self, func_info: &mut FuncInfo<'src>) -> Self {
        self.push_polytype(func_info.type_.clone())
            .instantiated()
            .and_then(|(l, func_ty)| {
                let Type::Function(args_ty, ret_ty) = func_ty else {
                    unreachable!();
                };
                Ok(((l, args_ty), ret_ty.as_ref().clone()))
            })
            .mutate_env(|env, ((_, args_ty), ret_ty)| {
                for (i, ty) in args_ty.iter().enumerate() {
                    let var_info = func_info.args.get(i).unwrap();
                    env.symbol_stack.push((
                        var_info.name,
                        LetType::Mono {
                            mutable: var_info.mutable,
                            inner: ty.clone(),
                        },
                    ));
                }
                env.func_ret_stack.push(ret_ty.clone())
            })
            .infer_type(func_info.body.as_mut().unwrap())
            .mutate_env(|env, (((_, args_ty), _), _)| {
                for _ in args_ty.iter() {
                    env.symbol_stack.pop();
                }
                env.func_ret_stack.pop();
            })
            .unify(
                |(_, body_ty)| body_ty.1,
                |((_, ret_ty), body_ty)| (ret_ty, &body_ty.0),
            )
            .push_any(func_info.type_.clone())
            .instantiated()
            .and_then(|((((x, args_ty), ret_ty), _), func_ty)| {
                let inferred_ty = Type::Function(args_ty, Box::new(ret_ty));
                let offset = inferred_ty.next_free_var();
                let inferred_ty = inferred_ty.instantiate(&Vec::new(), offset);
                inferred_ty
                    .is_generalization_of(&func_ty)
                    .map_err(|err| err.to_semantic_error(func_info.span, &func_ty, &inferred_ty))?;
                Ok(x)
            })
    }

    fn process_functions(mut self, func_table: &mut Vec<FuncInfo<'src>>) -> Self {
        for func_info in func_table.iter_mut() {
            self = self.process_function(func_info)
        }
        self
    }

    fn substitute<F: FnOnce(&I) -> Result<'src, Subst<'src>>>(self, f: F) -> Self {
        TypeEnvMonad(self.0.and_then(|st| {
            let subst = f(&st.val)?;
            Ok(st.substitute(&subst))
        }))
    }

    fn unify<S, F>(self, get_span: S, f: F) -> Self
    where
        S: FnOnce(&I) -> SimpleSpan,
        F: FnOnce(&I) -> (&Type<'src>, &Type<'src>),
    {
        self.substitute(|val| {
            let (ty1, ty2) = f(val);
            ty1.unify(ty2)
                .map_err(|err| err.to_semantic_error(get_span(val), ty1, ty2))
        })
    }

    fn unify_cloned<S, F>(self, get_span: S, f: F) -> Self
    where
        S: FnOnce(&I) -> SimpleSpan,
        F: FnOnce(I) -> (Type<'src>, Type<'src>),
    {
        self.substitute(|val| {
            let (ty1, ty2) = f(val.clone());
            ty1.unify(&ty2)
                .map_err(|err| err.to_semantic_error(get_span(val), &ty1, &ty2))
        })
    }

    fn mutate_env<F: FnOnce(&mut TypeEnv<'src>, &I)>(self, f: F) -> Self {
        TypeEnvMonad(self.0.map(|mut st| {
            f(&mut st.env, &st.val);
            st
        }))
    }

    fn gen_fresh_type(self) -> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = Type::Variable(st.tvg.next());
            st.map_val(|st_val| (st_val, new_ty))
        }))
    }

    fn gen_fresh_types(self, count: usize) -> TypeEnvMonad<'g, 'src, (I, Vec<Type<'src>>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_vec = (0..count)
                .map(|_| Type::Variable(st.tvg.next()))
                .collect::<Vec<_>>();
            st.map_val(|st_val| (st_val, new_vec))
        }))
    }

    fn push_any<R>(self, x: R) -> TypeEnvMonad<'g, 'src, (I, R)> {
        TypeEnvMonad(self.0.map(|st| st.map_val(|st_val| (st_val, x))))
    }

    fn push_type(self, ty: Type<'src>) -> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
        self.push_any(ty)
    }

    fn push_polytype(self, ty: Polytype<'src>) -> TypeEnvMonad<'g, 'src, (I, Polytype<'src>)> {
        self.push_any(ty)
    }

    fn push_from_env<R, F: FnOnce(&TypeEnv<'src>) -> R>(
        self,
        f: F,
    ) -> TypeEnvMonad<'g, 'src, (I, R)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_val = f(&st.env);
            st.map_val(|st_val| (st_val, new_val))
        }))
    }

    fn map<R, F: FnOnce(I) -> R>(self, f: F) -> TypeEnvMonad<'g, 'src, R> {
        TypeEnvMonad(self.0.map(|st| st.map_val(f)))
    }

    fn and_then<R, F: FnOnce(I) -> Result<'src, R>>(self, f: F) -> TypeEnvMonad<'g, 'src, R> {
        TypeEnvMonad(self.0.and_then(|st: TypeEnvMonadState<'g, 'src, I>| {
            Ok(TypeEnvMonadState {
                tvg: st.tvg,
                env: st.env,
                subst: st.subst,
                val: f(st.val)?,
            })
        }))
    }

    fn return_unit(self, span: SimpleSpan) -> Result<'src, (Subst<'src>, Spanned<Type<'src>>)> {
        self.0.map(|st| (st.subst, Spanned(Type::unit(), span)))
    }

    fn return_never(self, span: SimpleSpan) -> Result<'src, (Subst<'src>, Spanned<Type<'src>>)> {
        self.0.map(|st| {
            let never_val = Type::never(st.tvg);
            (st.subst, Spanned(never_val, span))
        })
    }
}

impl<'src> TypeEnv<'src> {
    pub fn new(func_type_list: Vec<Polytype<'src>>) -> Self {
        TypeEnv {
            symbol_stack: Vec::new(),
            func_type_list,
            func_ret_stack: Vec::new(),
        }
    }

    pub fn generalize(&self, ty: &Type<'src>) -> Polytype<'src> {
        Polytype {
            vars: ty
                .get_free_vars()
                .difference(&self.get_free_vars())
                .cloned()
                .collect(),
            ty: ty.clone(),
        }
    }

    fn monad<'g>(&'g mut self, tvg: &'g mut TypeVarGen) -> TypeEnvMonad<'g, 'src, ()> {
        TypeEnvMonad(Ok(TypeEnvMonadState {
            tvg: tvg,
            env: self,
            subst: Subst::new(),
            val: (),
        }))
    }

    fn infer_type_reference(&self, reference: &Reference, tvg: &mut TypeVarGen) -> Type<'src> {
        match reference {
            Reference::Function(index) => self.func_type_list.get(*index).unwrap().instantiate(tvg),
            Reference::Variable(index) => match &self
                .symbol_stack
                .get(self.symbol_stack.len() - *index - 1)
                .unwrap()
            {
                (name, LetType::Poly(inner)) => Type::LValue {
                    mutable: false,
                    def_ident: *name,
                    inner: Box::new(inner.instantiate(tvg)),
                },
                (name, LetType::Mono { mutable, inner }) => Type::LValue {
                    mutable: *mutable,
                    def_ident: *name,
                    inner: Box::new(inner.clone()),
                },
            },
        }
    }

    fn infer_type_impl(
        &mut self,
        node: &mut Node<'src>,
        tvg: &mut TypeVarGen,
    ) -> Result<'src, (Subst<'src>, Spanned<Type<'src>>)> {
        let node_span = node.span();
        let result = match node.tree_mut() {
            Tree::Constant { value } => Ok((
                Subst::new(),
                Spanned(
                    match value {
                        Value::RValue(rvalue) => match rvalue {
                            RValue::Unit => Type::unit(),
                            RValue::Number(_) => i32::get_type(),
                            RValue::Boolean(_) => bool::get_type(),
                            RValue::Reference(reference) => {
                                self.infer_type_reference(reference, tvg)
                            }
                        },
                        Value::LValue(reference) => self.infer_type_reference(reference, tvg),
                    },
                    node_span,
                ),
            )),
            Tree::Scope { nodes } if nodes.is_empty() => {
                Ok((Subst::new(), Spanned(Type::unit(), node_span)))
            }
            Tree::Scope { nodes } => {
                let start_len = self.symbol_stack.len();
                self.monad(tvg)
                    .infer_types(nodes)
                    .mutate_env(|env, _| env.symbol_stack.truncate(start_len))
                    .map(|(l, tys)| (l, tys.into_iter().last().unwrap()))
                    .return_last()
            }
            Tree::Call { callee, args } => self
                .monad(tvg)
                .infer_type(callee)
                .and_then(|st| match &st.1.0 {
                    Type::Function(args_ty, _) => {
                        if args.len() == args_ty.len() {
                            Ok((st.clone(), args_ty.clone()))
                        } else {
                            Err(Box::new(ArgumentCountMismatchError {
                                callee_span: callee.span(),
                                expected_count: args_ty.len(),
                                actual_count: args.len(),
                            }))
                        }
                    }
                    _ => Ok((st, Vec::new())), // will error later
                })
                .infer_types(args)
                .and_then(|st| {
                    if !st.0.1.is_empty() {
                        for (ty1, ty2) in st.0.1.iter().zip(st.1.iter()) {
                            // discard Subst just for errors
                            ty1.unify(&ty2.0)
                                .map_err(|err| err.to_semantic_error(ty2.1, &ty1, &ty2.0))?;
                        }
                    }
                    Ok((st.0.0, st.1))
                })
                .gen_fresh_type()
                .spanned(node_span)
                .unify_cloned(
                    |_| callee.span(),
                    |(((_, callee_ty), args_vec), ret_ty)| {
                        (
                            callee_ty.0,
                            Type::Function(
                                args_vec.into_iter().map(|ty| ty.0).collect(),
                                Box::new(ret_ty.0),
                            ),
                        )
                    },
                )
                .return_last(),
            Tree::Assign { assignee, value } => self
                .monad(tvg)
                .infer_type(assignee)
                .and_then(|st| match &st.1.0 {
                    Type::LValue {
                        mutable,
                        def_ident,
                        inner: _,
                    } => {
                        if *mutable {
                            Ok(st)
                        } else {
                            Err(Box::new(ImmutableAssignmentError {
                                assign_span: node_span,
                                var_def: *def_ident,
                            }))
                        }
                    }
                    _ => Err(Box::new(IllegalAssignmentError {
                        span: assignee.span(),
                    })),
                })
                .infer_type(value)
                .unify(
                    |(_, ty)| ty.1,
                    |((_, lhs_ty), rhs_ty)| (&lhs_ty.0, &rhs_ty.0),
                )
                .return_unit(node_span),
            Tree::Let {
                variable,
                value,
            } => {
                let monad = self
                    .monad(tvg)
                    .push_polytype(variable.ty.clone())
                    .instantiated()
                    .infer_type(value)
                    .unify(|(_, ty)| ty.1, |((_, val_ty), var_ty)| (val_ty, &var_ty.0));
                if variable.info.mutable {
                    monad
                        .mutate_env(|env, (_, ty)| {
                            env.symbol_stack.push((
                                variable.info.name,
                                LetType::Mono {
                                    mutable: true,
                                    inner: ty.0.clone(),
                                },
                            ))
                        })
                        .return_unit(node_span)
                } else {
                    monad
                        .generalized()
                        .mutate_env(|env, (_, ty)| {
                            env.symbol_stack
                                .push((variable.info.name, LetType::Poly(ty.0.clone())))
                        })
                        .return_unit(node_span)
                }
            }
            Tree::If {
                condition,
                if_then,
                if_else,
            } => self
                .monad(tvg)
                .infer_type(condition)
                .unified_with(Type::Builtin(BuiltinType::Bool))
                .infer_type(if_then)
                .infer_type(if_else)
                .unify(|_| if_then.span(), |((_, t1), t2)| (&t2.0, &t1.0))
                .return_last(),
            Tree::While { condition, body } => self
                .monad(tvg)
                .infer_type(condition)
                .unified_with(Type::Builtin(BuiltinType::Bool))
                .infer_type(body)
                .unified_with(Type::unit())
                .return_unit(node_span),
            Tree::Return { value } => self
                .monad(tvg)
                .infer_type(value)
                .push_from_env(|env| env.func_ret_stack.last().unwrap().clone())
                .unify(|((_, t1), _)| t1.1, |((_, t1), t2)| (&t1.0, t2))
                .return_never(node_span),
            Tree::Break => self.monad(tvg).return_never(node_span),
            Tree::Continue => self.monad(tvg).return_never(node_span),
        }?;
        node.set_type(result.1.0.clone());
        Ok(result)
    }

    pub fn infer_type(&mut self, ir: &mut IR<'src>) -> Result<'src, Type<'src>> {
        let mut tvg = TypeVarGen::new();
        self.monad(&mut tvg)
            .process_functions(ir.function_table_mut())
            .infer_type(ir.root_mut())
            .return_last()
            .map(|(_, ty)| ty.0)
    }
}
