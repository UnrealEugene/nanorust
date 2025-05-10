use core::{borrow::Borrow, cell::RefCell, iter::zip, ops::Deref};

use alloc::{
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use chumsky::span::SimpleSpan;
use hashbrown::HashMap;

use crate::{
    expr::Expr,
    parser::Variable,
    span::Spanned,
    value::{CValue, Flow, Function, Pointer, Value},
};

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
            .filter(|(_, func_ind, _)| func_ind.is_none())
            .position(|(var2, _, _)| var == *var2)
    }
}

struct FunStack<'src> {
    fun_scopes: Vec<HashMap<&'src str, (SimpleSpan, usize)>>,
    fun_list: Vec<Rc<Function<'src>>>,
}

impl<'src> FunStack<'src> {
    fn new() -> Self {
        FunStack {
            fun_scopes: Vec::new(),
            fun_list: Vec::new(),
        }
    }

    fn to_sym_stack(&self) -> SymStack<'src> {
        let mut res = SymStack::new();
        for scopes in self.fun_scopes.iter() {
            for (name, (_, index)) in scopes.iter() {
                res.push(name, Some(*index), false);
            }
        }
        res
    }

    fn push_scope(&mut self, expr: &Spanned<Expr<'src>>, sym_stack: &mut SymStack<'src>) {
        let mut scope = HashMap::new();
        match &expr.0 {
            Expr::Let { .. } => unreachable!(),
            Expr::VarScope { cont, .. } => self.push_scope(&*cont, sym_stack),
            Expr::Function { .. } => unreachable!(),
            Expr::FunScope {
                name,
                index,
                func,
                cont,
            } => {
                let i = self.fun_list.len();
                index.set(i);
                scope
                    .entry(name.0)
                    .and_modify(|_| panic!("the function `{}` is defined multiple times", name.0))
                    .or_insert_with(|| (name.1, i));
                self.fun_list.push(func.clone());
                sym_stack.push(name.0, Some(i), false);
                self.push_scope(cont, sym_stack);
            }
            _ => {}
        };
        self.fun_scopes.push(scope);
    }

    fn pop_scope(&mut self, sym_stack: &mut SymStack<'src>) {
        for _ in 0..self.fun_scopes.last().unwrap().len() {
            sym_stack.pop();
        }
        self.fun_scopes.pop();
    }

    fn to_function_list(self) -> Vec<Rc<Function<'src>>> {
        self.fun_list
    }
}

impl<'src> Expr<'src> {
    fn set_up_impl<'a>(
        spanned_self: &'a Spanned<Self>,
        sym_stack: &mut SymStack<'src>,
        fun_stack: &mut FunStack<'src>,
        cap_stack: &mut Vec<&'a RefCell<HashMap<String, Pointer<'src>>>>,
    ) {
        let _ = Expr::walk(spanned_self, |expr| match &expr.0 {
            Expr::Block(expr) => {
                fun_stack.push_scope(expr.as_ref(), sym_stack);
                Self::set_up_impl(expr.as_ref(), sym_stack, fun_stack, cap_stack);
                fun_stack.pop_scope(sym_stack);
                false
            }
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
                Self::set_up_impl(expr.as_ref(), sym_stack, fun_stack, cap_stack);
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
                Self::set_up_impl(val.as_ref(), sym_stack, fun_stack, cap_stack);
                sym_stack.push(var.name.0, None, *is_mut);
                Self::set_up_impl(cont.as_ref(), sym_stack, fun_stack, cap_stack);
                sym_stack.pop();
                false
            }
            Expr::FunScope { func, cont, .. } => {
                let Function { args, body, .. } = func.deref();
                // disallow accessing variables outside of function
                let mut new_sym_stack = fun_stack.to_sym_stack();
                let mut new_cap_stack = Vec::new();
                for name in args.iter() {
                    new_sym_stack.push(name.0, None, false);
                }
                Self::set_up_impl(
                    body.as_ref(),
                    &mut new_sym_stack,
                    fun_stack,
                    &mut new_cap_stack,
                );
                Self::set_up_impl(cont.as_ref(), sym_stack, fun_stack, cap_stack);
                false
            }
            Expr::Closure(func, captures) => {
                let Function { args, body, .. } = func.deref();
                sym_stack.push_closure();
                for name in args.iter() {
                    sym_stack.push(name.0, None, false);
                }
                cap_stack.push(captures);
                Self::set_up_impl(body.as_ref(), sym_stack, fun_stack, cap_stack);
                sym_stack.pop_closure();
                cap_stack.pop();
                false
            }
            Expr::For {
                var: Variable { name, .. },
                iter,
                body,
            } => {
                Self::set_up_impl(iter.as_ref(), sym_stack, fun_stack, cap_stack);
                sym_stack.push(name.0, None, false);
                Self::set_up_impl(body.as_ref(), sym_stack, fun_stack, cap_stack);
                sym_stack.pop();
                false
            }
            _ => true,
        });
    }

    pub fn set_up<'a>(spanned_self: &'a Spanned<Self>) -> Environment<'src> {
        let mut sym_stack: SymStack<'src> = SymStack::new();
        let mut fun_stack: FunStack<'src> = FunStack::new();
        fun_stack.push_scope(spanned_self, &mut sym_stack);
        let mut cap_stack: Vec<&'a RefCell<HashMap<String, Pointer<'src>>>> = Vec::new();
        Self::set_up_impl(spanned_self, &mut sym_stack, &mut fun_stack, &mut cap_stack);
        Environment {
            stack: EvalStack::new(),
            functions: fun_stack.to_function_list(),
        }
    }
}

#[derive(Default)]
pub struct EvalStack<'src>(Vec<(&'src str, Value<'src>)>);

#[derive(Default)]
pub struct Environment<'src> {
    pub stack: EvalStack<'src>,
    pub functions: Vec<Rc<Function<'src>>>,
}

impl<'src> EvalStack<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_relative(&self, i: usize) -> Option<&Value<'src>> {
        self.0.get(self.len() - i - 1).map(|e| &e.1)
    }

    pub fn get(&self, ptr: Pointer<'src>) -> Option<&Value<'src>> {
        ptr.to_absolute(&self)
            .try_unwrap_absolute()
            .and_then(|i| self.0.get(i))
            .map(|e| &e.1)
    }

    pub fn index<Q>(&self, k: &Q) -> Option<usize>
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

    pub fn assign(&mut self, ptr: Pointer<'src>, v: Value<'src>) {
        let index = ptr.to_absolute(&self).unwrap_absolute();
        self.0.get_mut(index).map(|e| e.1 = v);
    }

    pub fn push(&mut self, k: &'src str, v: Value<'src>) {
        self.0.push((k, v));
    }

    pub fn pop(&mut self) -> Option<(&'src str, Value<'src>)> {
        self.0.pop()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'src> Environment<'src> {
    pub fn get(&self, ptr: Pointer<'src>) -> Option<Value<'src>> {
        self.stack.get(ptr).cloned().or_else(|| match ptr {
            Pointer::Function(i) => Some(Value::Function(i)),
            _ => None,
        })
    }
}

impl<'src> Expr<'src> {
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
                    let func = val.to_function(env);
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
}
