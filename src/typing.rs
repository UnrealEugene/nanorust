// Algorithm W implementation of type inference in Hindley-Milner type system.
// Based on implementation from https://github.com/nwoeanhinnogaehr/algorithmw-rust

use core::fmt::{self, Display};
use core::hash::Hash;
use core::result;

use alloc::rc::Rc;
use alloc::string::String;
use alloc::{boxed::Box, vec::Vec};
use alloc::{format, vec};

use hashbrown::{HashMap, HashSet};
use nanorust_macros::ty;

use crate::parser::Identifier;
use crate::value::{Function, Value};
use crate::{expr::Expr, span::Spanned};

pub type Result<T> = result::Result<T, TypeError>;

pub struct TypeError {
    msg: String,
}

impl TypeError {
    fn new<S>(msg: S) -> TypeError
    where
        String: From<S>,
    {
        TypeError {
            msg: String::from(msg),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.msg.fmt(f)
    }
}

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
    fn bind<'src>(&self, ty: &Type<'src>) -> Result<Subst<'src>> {
        if let Type::Variable(u) = ty {
            if u == self {
                return Ok(Subst::new());
            }
        }
        if ty.get_free_vars().contains(self) {
            return Err(TypeError::new(format!(
                "occur check fails: {} vs {}",
                self, ty
            )));
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum Type<'src> {
    Builtin(BuiltinType),
    Concrete(Identifier<'src>),
    Variable(TypeVar),
    Reference {
        is_mut: bool,
        ty: Box<Self>,
    },
    Tuple(Vec<Self>),
    Function(Vec<Self>, Box<Self>),
    LValue {
        is_mut: bool,
        ty: Box<Self>,
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
            Type::Reference { is_mut, ty } => {
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
            Type::LValue { is_mut: _, ty } => ty.fmt(f),
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
            Type::Reference { ty, .. } => ty.get_free_vars(),
            Type::Tuple(ts) => ts.get_free_vars(),
            Type::Function(args, ret) => args
                .get_free_vars()
                .union(&ret.get_free_vars())
                .cloned()
                .collect(),
            Type::LValue { is_mut: _, ty } => ty.get_free_vars(),
            Type::Unknown => unreachable!(),
        }
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        match self {
            Type::Builtin(..) => self.clone(),
            Type::Concrete(..) => self.clone(),
            Type::Variable(i) => subst.0.get(i).unwrap_or(self).clone(),
            Type::Reference { is_mut, ty, .. } => Type::Reference {
                is_mut: *is_mut,
                ty: Box::new(ty.substitute(subst)),
            },
            Type::Tuple(ts) => Type::Tuple(ts.substitute(subst)),
            Type::Function(args, ret) => {
                Type::Function(args.substitute(subst), Box::new(ret.substitute(subst)))
            }
            Type::LValue { is_mut, ty } => Type::LValue {
                is_mut: *is_mut,
                ty: Box::new(ty.substitute(subst)),
            },
            Type::Unknown => unreachable!(),
        }
    }
}

impl<'src> Type<'src> {
    pub fn has_unknowns(&self) -> bool {
        match self {
            Type::Reference { ty, .. } => ty.has_unknowns(),
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
            Type::LValue { ty, .. } => ty.has_unknowns(),
            Type::Unknown => true,
            _ => false,
        }
    }

    fn replace_unknowns(self, gen: &mut TypeVarGen) -> Self {
        match self {
            Type::Builtin(_) => self,
            Type::Concrete(_) => self,
            Type::Variable(_) => unreachable!(),
            Type::Reference { is_mut, ty } => Type::Reference {
                is_mut,
                ty: Box::new(ty.replace_unknowns(gen)),
            },
            Type::Tuple(elems) => {
                Type::Tuple(elems.into_iter().map(|e| e.replace_unknowns(gen)).collect())
            }
            Type::Function(args, ret) => Type::Function(
                args.into_iter().map(|e| e.replace_unknowns(gen)).collect(),
                Box::new(ret.replace_unknowns(gen)),
            ),
            Type::LValue { is_mut, ty } => Type::LValue {
                is_mut,
                ty: Box::new(ty.replace_unknowns(gen)),
            },
            Type::Unknown => Type::Variable(gen.next()),
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
            Type::Reference { is_mut, ty } => Type::Reference {
                is_mut,
                ty: Box::new(ty.instantiate(type_vars, offset)),
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
            Type::LValue { is_mut, ty } => Type::LValue {
                is_mut,
                ty: Box::new(ty.instantiate(type_vars, offset)),
            },
            Type::Unknown => self,
        }
    }

    fn next_free_var(&self) -> usize {
        match self {
            Type::Builtin(_) => 0,
            Type::Concrete(_) => 0,
            Type::Variable(TypeVar(i)) => i + 1,
            Type::Reference { ty, .. } => ty.next_free_var(),
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
            Type::LValue { ty, .. } => ty.next_free_var(),
            Type::Unknown => 0,
        }
    }

    pub fn unit() -> Self {
        Type::Tuple(Vec::new())
    }

    fn never(gen: &mut TypeVarGen) -> Self {
        Type::Variable(gen.next())
    }

    fn unify_vec(first: &Vec<Self>, second: &Vec<Self>) -> Result<Subst<'src>> {
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

    fn unify(&self, other: &Self) -> Result<Subst<'src>> {
        match (self, other) {
            (Type::LValue { is_mut: _, ty: ty1 }, ty2) => ty1.unify(ty2),
            (ty1, Type::LValue { is_mut: _, ty: ty2 }) => ty1.unify(ty2),
            (Type::Builtin(t1), Type::Builtin(t2)) if t1 == t2 => Ok(Subst::new()),
            (Type::Concrete(t1), Type::Concrete(t2)) if t1.0 == t2.0 => Ok(Subst::new()),
            (Type::Variable(var), t) => var.bind(t),
            (
                Type::Reference {
                    is_mut: is_mut1,
                    ty: ty1,
                },
                Type::Reference {
                    is_mut: is_mut2,
                    ty: ty2,
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
            (ty1, ty2) => Err(TypeError::new(format!("type mismatch: {} vs {}", ty1, ty2))),
        }
    }

    fn cast_unify(&self, other: &Self) -> Result<Subst<'src>> {
        match (self, other) {
            (Type::Builtin(..), Type::Builtin(..)) => Ok(Subst::new()),
            (
                Type::Reference {
                    is_mut: is_mut1,
                    ty: ty1,
                },
                Type::Reference {
                    is_mut: is_mut2,
                    ty: ty2,
                },
            ) if !is_mut2 || *is_mut1 => ty1.unify(ty2),
            (Type::LValue { is_mut: _, ty: ty1 }, ty2) => ty1.cast_unify(ty2),
            (ty1, Type::LValue { is_mut: _, ty: ty2 }) => ty1.cast_unify(ty2),
            (ty1, ty2) => Err(TypeError::new(format!(
                "non-primitive cast: {} into {}",
                ty1, ty2
            ))),
        }
    }

    // fn is_generalization_of_vec(
    //     left: &Vec<Type<'src>>,
    //     right: &Vec<Type<'src>>,
    // ) -> Result<Subst<'src>> {
    //     left.iter()
    //         .zip(right.iter())
    //         .fold(Ok(Subst::new()), |res, (t1, t2)| {
    //             res.and_then(|subst1| {
    //                 t1.is_generalization_of(t2)
    //                     .and_then(|subst2| subst1.strict_compose(&subst2))
    //             })
    //         })
    // }

    // fn is_generalization_of(&self, other: &Type<'src>) -> Result<Subst<'src>> {
    //     match (self, other) {
    //         (Type::LValue { is_mut: _, ty: ty1 }, ty2) => ty1.is_generalization_of(ty2),
    //         (ty1, Type::LValue { is_mut: _, ty: ty2 }) => ty1.is_generalization_of(ty2),
    //         (Type::Builtin(t1), Type::Builtin(t2)) if t1 == t2 => Ok(Subst::new()),
    //         (Type::Concrete(t1), Type::Concrete(t2)) if t1.0 == t2.0 => Ok(Subst::new()),
    //         (Type::Variable(var), t) => var.bind(t),
    //         (t, Type::Variable(_)) => Err(TypeError::new(format!(
    //             "expected a type variable, found {}",
    //             t
    //         ))),
    //         (
    //             Type::Reference {
    //                 is_mut: is_mut1,
    //                 ty: ty1,
    //             },
    //             Type::Reference {
    //                 is_mut: is_mut2,
    //                 ty: ty2,
    //             },
    //         ) if is_mut1 == is_mut2 => ty1.is_generalization_of(ty2),
    //         (Type::Tuple(ts1), Type::Tuple(ts2)) if ts1.len() == ts2.len() => {
    //             Self::is_generalization_of_vec(ts1, ts2)
    //         }
    //         (Type::Function(args1, ret1), Type::Function(args2, ret2))
    //             if args1.len() == args2.len() =>
    //         {
    //             let subst1 = Self::is_generalization_of_vec(args1, args2)?;
    //             let subst2 = ret1.is_generalization_of(ret2)?;
    //             subst1.strict_compose(&subst2)
    //         }
    //         (ty1, ty2) => Err(TypeError::new(format!(
    //             "type mismatch 2: {} vs {}",
    //             ty1, ty2
    //         ))),
    //     }
    // }

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
    fn instantiate(&self, gen: &mut TypeVarGen) -> Type<'src> {
        self.ty.substitute(&Subst(
            self.vars
                .iter()
                .cloned()
                .zip(self.vars.iter().map(|_| Type::Variable(gen.next())))
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
        let mut gen = TypeVarGen::new();
        let new_ty = ty.replace_unknowns(&mut gen);
        Polytype {
            vars: new_ty.get_free_vars().iter().cloned().collect(),
            ty: new_ty,
        }
    }

    pub fn bind_params(&self, bindings: &Vec<Type<'src>>) -> Self {
        let offset = self.ty.next_free_var();
        let mut gen = TypeVarGen::new_from_offset(offset);
        let subst = Subst(
            self.vars
                .iter()
                .cloned()
                .zip(
                    bindings
                        .iter()
                        .cloned()
                        .map(|ty| ty.replace_unknowns(&mut gen)),
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
    Let(Polytype<'src>),
    LetMut(Type<'src>),
}

impl<'src> HMType<'src> for LetType<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        match self {
            LetType::Let(ty) => ty.get_free_vars(),
            LetType::LetMut(ty) => ty.get_free_vars(),
        }
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        match self {
            LetType::Let(ty) => LetType::Let(ty.substitute(subst)),
            LetType::LetMut(ty) => LetType::LetMut(ty.substitute(subst)),
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

    // fn strict_compose(&self, other: &Self) -> Result<Self> {
    //     let mut new_self = self.clone();
    //     for (var, ty2) in other.0.iter() {
    //         if !self.0.contains_key(var) {
    //             new_self.0.insert(*var, ty2.clone());
    //         } else {
    //             let ty1 = self.0.get(var).unwrap();
    //             if ty1 != ty2 {
    //                 return Err(TypeError::new(format!(
    //                     "type mismatch in generalization check: {} vs {}",
    //                     ty1, ty2
    //                 )));
    //             }
    //         }
    //     }
    //     Ok(new_self)
    // }

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
    sym_stack: Vec<LetType<'src>>,
    fun_type_list: Vec<Polytype<'src>>,
    fun_ret_stack: Vec<Type<'src>>,
}

impl<'src> HMType<'src> for TypeEnv<'src> {
    fn get_free_vars(&self) -> HashSet<TypeVar> {
        self.sym_stack.clone().get_free_vars()
    }

    fn substitute(&self, subst: &Subst<'src>) -> Self {
        TypeEnv {
            sym_stack: self.sym_stack.iter().map(|t| t.substitute(subst)).collect(),
            fun_type_list: self
                .fun_type_list
                .iter()
                .map(|t| t.substitute(subst))
                .collect(),
            fun_ret_stack: self
                .fun_ret_stack
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

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Polytype<'src>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, f: F) -> Self {
        (self.0.map(f), self.1)
    }
}

impl<'src, I: TupleChain<'src>> TupleChain<'src> for (I, Vec<Type<'src>>) {
    fn map<F: FnMut(Type<'src>) -> Type<'src>>(self, mut f: F) -> Self {
        (self.0.map(&mut f), self.1.into_iter().map(f).collect())
    }
}

struct TypeEnvMonadState<'g, 'src, I> {
    gen: &'g mut TypeVarGen,
    env: TypeEnv<'src>,
    subst: Subst<'src>,
    val: I,
}

impl<'g, 'src, I> TypeEnvMonadState<'g, 'src, I> {
    fn map_val<R, F: FnOnce(I) -> R>(self, f: F) -> TypeEnvMonadState<'g, 'src, R> {
        TypeEnvMonadState {
            gen: self.gen,
            env: self.env,
            subst: self.subst,
            val: f(self.val),
        }
    }
}

impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonadState<'g, 'src, I> {
    fn substitute(self, subst: &Subst<'src>) -> Self {
        TypeEnvMonadState {
            gen: self.gen,
            env: self.env.substitute(&subst),
            subst: subst.compose(&self.subst),
            val: self.val.map(|t| t.substitute(subst)),
        }
    }
}

struct TypeEnvMonad<'g, 'src, I>(Result<TypeEnvMonadState<'g, 'src, I>>);

impl<'g, 'src> TypeEnvMonad<'g, 'src, Type<'src>> {
    fn return_last(self) -> Result<(Subst<'src>, Type<'src>)> {
        self.0.map(|st| {
            let new_val = st.val.substitute(&st.subst);
            (st.subst, new_val)
        })
    }
}

impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
    fn generalized(self) -> TypeEnvMonad<'g, 'src, (I, Polytype<'src>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = st.env.generalize(&st.val.1);
            st.map_val(|(l, _)| (l, new_ty))
        }))
    }

    fn unified_with(self, ty: Type<'src>) -> Self {
        self.substitute(|val| val.1.unify(&ty))
    }

    fn return_last(self) -> Result<(Subst<'src>, Type<'src>)> {
        self.0.map(|st| {
            let new_val = st.val.1.substitute(&st.subst);
            (st.subst, new_val)
        })
    }
}

impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, (I, Polytype<'src>)> {
    fn instantiated(self) -> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
        TypeEnvMonad(self.0.map(|st| {
            let new_ty = st.val.1.instantiate(st.gen);
            st.map_val(|(l, _)| (l, new_ty))
        }))
    }
}

impl<'g, 'src, I: TupleChain<'src>> TypeEnvMonad<'g, 'src, I> {
    fn infer_type(self, expr: &Spanned<Expr<'src>>) -> TypeEnvMonad<'g, 'src, (I, Type<'src>)> {
        TypeEnvMonad(self.0.and_then(|st| {
            let (subst, ty) = st.env.infer_type_impl(expr, st.gen)?;
            Ok(st.substitute(&subst).map_val(|st_val| (st_val, ty)))
        }))
    }

    fn infer_types(
        self,
        exprs: &Vec<Spanned<Expr<'src>>>,
    ) -> TypeEnvMonad<'g, 'src, (I, Vec<Type<'src>>)> {
        TypeEnvMonad(
            exprs
                .iter()
                .fold(self.0.map(|st| (st, Vec::new())), |st, expr| {
                    let (st, mut vec) = st?;
                    let (subst, ty) = st.env.infer_type_impl(expr, st.gen)?;
                    vec.push(ty);
                    Ok((st.substitute(&subst), vec))
                })
                .map(|(st, vec)| st.map_val(|st_val| (st_val, vec))),
        )
    }

    fn substitute<F: FnOnce(&I) -> Result<Subst<'src>>>(self, f: F) -> Self {
        TypeEnvMonad(self.0.and_then(|st| {
            let subst = f(&st.val)?;
            Ok(st.substitute(&subst))
        }))
    }

    fn unify<F: FnOnce(&I) -> (&Type<'src>, &Type<'src>)>(self, f: F) -> Self {
        self.substitute(|val| {
            let (t1, t2) = f(val);
            t1.unify(t2)
        })
    }

    fn unify_cloned<F: FnOnce(I) -> (Type<'src>, Type<'src>)>(self, f: F) -> Self {
        self.substitute(|val| {
            let (t1, t2) = f(val.clone());
            t1.unify(&t2)
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
            let new_ty = Type::Variable(st.gen.next());
            st.map_val(|st_val| (st_val, new_ty))
        }))
    }

    // fn gen_fresh_types(self, count: usize) -> TypeEnvMonad<'g, 'src, (I, Vec<Type<'src>>)> {
    //     TypeEnvMonad(self.0.map(|st| {
    //         let new_vec = (0..count)
    //             .map(|_| Type::Variable(st.gen.next()))
    //             .collect::<Vec<_>>();
    //         st.map_val(|st_val| (st_val, new_vec))
    //     }))
    // }

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

    fn and_then<R, F: FnOnce(I) -> Result<R>>(self, f: F) -> TypeEnvMonad<'g, 'src, R> {
        TypeEnvMonad(self.0.and_then(|st: TypeEnvMonadState<'g, 'src, I>| {
            Ok(TypeEnvMonadState {
                gen: st.gen,
                env: st.env,
                subst: st.subst,
                val: f(st.val)?,
            })
        }))
    }

    fn return_unit(self) -> Result<(Subst<'src>, Type<'src>)> {
        self.0.map(|st| (st.subst, Type::unit()))
    }

    fn return_never(self) -> Result<(Subst<'src>, Type<'src>)> {
        self.0.map(|st| {
            let never_val = Type::never(st.gen);
            (st.subst, never_val)
        })
    }
}

impl<'src> TypeEnv<'src> {
    pub fn new(fun_list: Vec<Rc<Function<'src>>>) -> Self {
        let mut fun_type_list = Vec::new();
        for func in fun_list.iter() {
            fun_type_list.push(func.ty.clone());
        }
        TypeEnv {
            sym_stack: Vec::new(),
            fun_type_list,
            fun_ret_stack: Vec::new(),
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

    fn monad<'g>(&self, gen: &'g mut TypeVarGen) -> TypeEnvMonad<'g, 'src, ()> {
        TypeEnvMonad(Ok(TypeEnvMonadState {
            gen: gen,
            env: self.clone(),
            subst: Subst::new(),
            val: (),
        }))
    }

    fn infer_type_impl(
        &self,
        expr: &Spanned<Expr<'src>>,
        gen: &mut TypeVarGen,
    ) -> Result<(Subst<'src>, Type<'src>)> {
        match &expr.0 {
            Expr::Error => unreachable!(),
            Expr::Skip => Ok((Subst::new(), Type::unit())),
            Expr::Block(inner) => self.infer_type_impl(inner, gen),
            // Expr::Ignore(inner) => self.monad(gen).infer_type(inner).return_unit(),
            Expr::Location {
                name: _name,
                // ptr_cell,
                bindings: _bindings,
            } => Ok((
                Subst::new(),
                // match ptr_cell.get().0.to_absolute_sym(self.sym_stack.len()) {
                //     Pointer::Absolute(i) => match self.sym_stack.get(i).unwrap() {
                //         LetType::Let(ty) => Type::LValue {
                //             is_mut: false,
                //             ty: Box::new(ty.bind_params(bindings).instantiate(gen)),
                //         },
                //         LetType::LetMut(ty) => {
                //             if bindings.len() > 0 {
                //                 return Err(TypeError::new(format!(
                //                     "cannot bind type parameters to a mutable variable `{}`",
                //                     name.0
                //                 )));
                //             }
                //             Type::LValue {
                //                 is_mut: true,
                //                 ty: Box::new(ty.clone()),
                //             }
                //         }
                //     },
                //     Pointer::Function(i) => self
                //         .fun_type_list
                //         .get(i)
                //         .unwrap()
                //         .bind_params(bindings)
                //         .instantiate(gen),
                //     _ => unreachable!(),
                // },
                Type::unit(),
            )),
            Expr::Constant(val) => Ok((
                Subst::new(),
                Type::Builtin(match val {
                    Value::Number(_) => BuiltinType::I32,
                    Value::Boolean(_) => BuiltinType::Bool,
                    _ => unreachable!(),
                }),
            )),
            Expr::Tuple(elems) => self
                .monad(gen)
                .infer_types(elems)
                .map(|(_, vec)| Type::Tuple(vec))
                .return_last(),
            Expr::Reference { is_mut, expr } => self
                .monad(gen)
                .infer_type(expr)
                .map(|(_, ty)| Type::Reference {
                    is_mut: *is_mut,
                    ty: Box::new(ty),
                })
                .return_last(),
            Expr::Dereference { is_mut, expr } => self
                .monad(gen)
                .push_polytype(if *is_mut {
                    ty!("fn(&mut '0) -> '0")
                } else {
                    ty!("fn(&'0) -> '0")
                })
                .instantiated()
                .infer_type(expr)
                .gen_fresh_type()
                .unify_cloned(|(((_, op_ty), arg_ty), ret_ty)| {
                    (op_ty, Type::Function(vec![arg_ty], Box::new(ret_ty)))
                })
                .map(|(_, ret_ty)| Type::LValue {
                    is_mut: *is_mut,
                    ty: Box::new(ret_ty),
                })
                .return_last(),
            Expr::Unary(op, arg) => self
                .monad(gen)
                .push_polytype(op.0.get_type())
                .instantiated()
                .infer_type(arg)
                .gen_fresh_type()
                .unify_cloned(|(((_, op_ty), arg_ty), ret_ty)| {
                    (op_ty, Type::Function(vec![arg_ty], Box::new(ret_ty)))
                })
                .return_last(),
            Expr::Binary(op, left, right) => self
                .monad(gen)
                .push_polytype(op.0.get_type())
                .instantiated()
                .infer_type(left)
                .infer_type(right)
                .gen_fresh_type()
                .unify_cloned(|((((_, op_ty), left_ty), right_ty), ret_ty)| {
                    (
                        op_ty,
                        Type::Function(vec![left_ty, right_ty], Box::new(ret_ty)),
                    )
                })
                .return_last(),
            Expr::Assign(lhs, rhs) => self
                .monad(gen)
                .infer_type(lhs)
                .and_then(|st| match &st.1 {
                    Type::LValue { is_mut, ty: _ } if *is_mut => Ok(st),
                    Type::LValue { .. } => Err(TypeError::new(
                        "cannot assign to value, which is behind a `&` reference",
                    )),
                    _ => Err(TypeError::new("cannot assign to this expression")),
                })
                .infer_type(rhs)
                .unify(|((_, lhs_ty), rhs_ty)| (lhs_ty, rhs_ty))
                .return_unit(),
            Expr::Seq(first, second) => self
                .monad(gen)
                .infer_type(first)
                .unified_with(Type::unit())
                .infer_type(second)
                .return_last(),
            Expr::Let { .. } => unreachable!(),
            // Expr::VarScope {
            //     var,
            //     is_mut,
            //     val,
            //     cont,
            // } => {
            //     let monad = self
            //         .monad(gen)
            //         .push_polytype(var.ty.borrow().clone())
            //         .instantiated()
            //         .infer_type(val)
            //         .unify(|((_, val_ty), var_ty)| (val_ty, var_ty));
            //     if *is_mut {
            //         monad
            //             .mutate_env(|env, (_, t)| env.sym_stack.push(LetType::LetMut(t.clone())))
            //             .infer_type(cont)
            //             .mutate_env(|env, _| {
            //                 env.sym_stack.pop();
            //             })
            //             .return_last()
            //     } else {
            //         monad
            //             .generalized()
            //             .mutate_env(|env, (_, t)| env.sym_stack.push(LetType::Let(t.clone())))
            //             .infer_type(cont)
            //             .mutate_env(|env, _| {
            //                 env.sym_stack.pop();
            //             })
            //             .return_last()
            //     }
            // }
            Expr::Function { .. } => unreachable!(),
            // Expr::FunScope {
            //     name: _,
            //     index,
            //     func,
            //     cont,
            // } => self
            //     .monad(gen)
            //     .gen_fresh_types(func.args.len())
            //     .gen_fresh_type()
            //     .mutate_env(|env, ((_, args_ty), ret_ty)| {
            //         for ty in args_ty.iter() {
            //             env.sym_stack.push(LetType::LetMut(ty.clone()));
            //         }
            //         env.fun_ret_stack.push(ret_ty.clone())
            //     })
            //     .infer_type(&func.body)
            //     .mutate_env(|env, (((_, args_ty), _), _)| {
            //         for _ in args_ty.iter() {
            //             env.sym_stack.pop();
            //         }
            //         env.fun_ret_stack.pop();
            //     })
            //     .unify(|((_, ret_ty), body_ty)| (ret_ty, body_ty))
            //     .push_from_env(|env| env.fun_type_list.get(index.get()).unwrap().clone())
            //     .instantiated()
            //     .and_then(|((((_, args_ty), ret_ty), _), func_ty)| {
            //         let inferred_ty = Type::Function(args_ty, Box::new(ret_ty));
            //         let offset = inferred_ty.next_free_var();
            //         let _ = inferred_ty
            //             .instantiate(&func.params, offset)
            //             .is_generalization_of(&func_ty)?;
            //         Ok(())
            //     })
            //     .infer_type(cont)
            //     .return_last(),
            Expr::Cast(arg, ty) => self
                .monad(gen)
                .infer_type(arg)
                .push_polytype(ty.clone())
                .instantiated()
                .substitute(|((_, lhs_ty), rhs_ty)| lhs_ty.cast_unify(rhs_ty))
                .return_last(),
            Expr::If {
                cond,
                if_true,
                if_false: _,
            } => self
                .monad(gen)
                .infer_type(cond)
                .unified_with(Type::Builtin(BuiltinType::Bool))
                .infer_type(if_true)
                // .infer_type(&Spanned::unwrap_or_default(if_false.as_ref().map(Box::as_ref), expr.1))
                .unify(|((_, t1), t2)| (t1, t2))
                .return_last(),
            Expr::Closure(func, _) => self
                .monad(gen)
                .push_polytype(func.ty.clone())
                .instantiated()
                .mutate_env(|env, (_, func_ty)| {
                    let (args_ty, ret_ty) = func_ty.unwrap_function_ref();
                    for ty in args_ty.iter() {
                        env.sym_stack.push(LetType::LetMut(ty.clone()));
                    }
                    env.fun_ret_stack.push(ret_ty.clone())
                })
                .infer_type(&func.body)
                .mutate_env(|env, ((_, func_ty), _)| {
                    let args_ty = func_ty.unwrap_function_ref().0;
                    for _ in args_ty.iter() {
                        env.sym_stack.pop();
                    }
                    env.fun_ret_stack.pop();
                })
                .unify(|((_, func_ty), body_ty)| (func_ty.unwrap_function_ref().1, body_ty))
                .map(|((_, func_ty), _)| func_ty)
                .return_last(),
            Expr::Call(callee, args) => self
                .monad(gen)
                .infer_type(callee)
                .infer_types(args)
                .gen_fresh_type()
                .unify_cloned(|(((_, callee_ty), args_vec), ret_ty)| {
                    (callee_ty, Type::Function(args_vec, Box::new(ret_ty)))
                })
                .return_last(),
            Expr::Return(expr) => self
                .monad(gen)
                .infer_type(expr)
                .push_from_env(|env| env.fun_ret_stack.last().unwrap().clone())
                .unify(|((_, t1), t2)| (t1, t2))
                .return_never(),
            Expr::While { cond, body } => self
                .monad(gen)
                .infer_type(cond)
                .unified_with(Type::Builtin(BuiltinType::Bool))
                .infer_type(body)
                .unified_with(Type::unit())
                .return_unit(),
            Expr::For { iter, body, .. } => self
                .monad(gen)
                .push_type(Type::Builtin(BuiltinType::I32))
                .generalized()
                .infer_type(iter)
                .unified_with(Type::Builtin(BuiltinType::Range))
                .mutate_env(|env, ((_, t), _)| env.sym_stack.push(LetType::Let(t.clone())))
                .infer_type(body)
                .mutate_env(|env, _| {
                    env.sym_stack.pop();
                })
                .unified_with(Type::unit())
                .return_unit(),
            Expr::Continue(_) => self.monad(gen).return_never(),
            Expr::Break(_) => self.monad(gen).return_never(),
        }
    }

    pub fn infer_type(&self, expr: &Spanned<Expr<'src>>) -> Result<Type<'src>> {
        let mut gen = TypeVarGen::new();
        self.infer_type_impl(expr, &mut gen).map(|(_, ty)| ty)
    }
}
