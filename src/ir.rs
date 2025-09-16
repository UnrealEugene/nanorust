use core::{iter::Rev, slice::Iter};

use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use chumsky::span::{SimpleSpan, Span};

use crate::{
    error::{BreakOutsideOfLoopError, FunctionCaptureError, SemanticError, UndefinedSymbolError},
    expr::Expr,
    parser::Identifier,
    span::Spanned,
};

pub type Result<'src, T> = core::result::Result<T, Box<dyn SemanticError + 'src>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
    Function(usize),
    Variable(usize),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum RValue {
    #[default]
    Unit,
    Number(i32),
    Boolean(bool),
    Reference(Reference),
}

impl Into<Value> for RValue {
    fn into(self) -> Value {
        Value::RValue(self)
    }
}

impl ToString for RValue {
    fn to_string(&self) -> String {
        match self {
            RValue::Unit => "()".into(),
            RValue::Number(n) => n.to_string(),
            RValue::Boolean(b) => b.to_string(),
            RValue::Reference(_) => "<ref>".into(),
        }
    }
}

impl RValue {
    pub fn number(self) -> Option<i32> {
        match self {
            RValue::Number(x) => Some(x),
            _ => None,
        }
    }

    pub fn boolean(self) -> Option<bool> {
        match self {
            RValue::Boolean(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    RValue(RValue),
    LValue(Reference),
}

impl Default for Value {
    fn default() -> Self {
        Self::RValue(RValue::default())
    }
}

impl Value {
    pub fn unit() -> Self {
        Self::default()
    }
}

pub struct FuncInfo<'src> {
    pub name: Identifier<'src>,
    pub args: Vec<Identifier<'src>>,
    pub span: SimpleSpan,
    pub global: bool,
    pub body: Option<Node<'src>>,
}

pub struct IR<'src> {
    func_table: Vec<FuncInfo<'src>>,
    root: Node<'src>,
}

enum IRSymbol<'src> {
    InaccesibleVariable,
    Variable,
    Function { index: usize, info: FuncInfo<'src> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolScopeType {
    Block,
    Frame,
}

struct SymbolScope<'src> {
    scope_type: SymbolScopeType,
    scope: Vec<(Identifier<'src>, IRSymbol<'src>)>,
}

impl<'src> SymbolScope<'src> {
    fn new(scope_type: SymbolScopeType) -> Self {
        Self {
            scope_type,
            scope: Vec::new(),
        }
    }

    fn push(&mut self, name: Identifier<'src>, symbol: IRSymbol<'src>) {
        self.scope.push((name, symbol));
    }

    fn pop(&mut self) -> Option<(Identifier<'src>, IRSymbol<'src>)> {
        self.scope.pop()
    }
}

struct SymbolStackIter<'a, 'src> {
    iter: Rev<Iter<'a, SymbolScope<'src>>>,
    inner_iter: Rev<Iter<'a, (Identifier<'src>, IRSymbol<'src>)>>,
    scope_type: SymbolScopeType,
    escaped_frame: bool,
}

impl<'a, 'src> Iterator for SymbolStackIter<'a, 'src> {
    type Item = (&'a Identifier<'src>, &'a IRSymbol<'src>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((name, symbol)) = self.inner_iter.next() {
                return Some((
                    name,
                    match symbol {
                        IRSymbol::Variable if self.escaped_frame => &IRSymbol::InaccesibleVariable,
                        _ => symbol,
                    },
                ));
            }
            if self.scope_type == SymbolScopeType::Frame {
                self.escaped_frame = true;
            }
            let Some(symbol_scope) = self.iter.next() else {
                return None;
            };
            self.inner_iter = symbol_scope.scope.iter().rev();
            self.scope_type = symbol_scope.scope_type;
        }
    }
}

struct SymbolStack<'src> {
    stack: Vec<SymbolScope<'src>>,
}

impl<'src> SymbolStack<'src> {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a Identifier<'src>, &'a IRSymbol<'src>)> {
        let mut iter = self.stack.iter().rev();
        let symbol_scope = iter.next().unwrap();
        SymbolStackIter {
            iter: iter,
            inner_iter: symbol_scope.scope.iter().rev(),
            scope_type: symbol_scope.scope_type,
            escaped_frame: false,
        }
    }

    fn last_scope_type(&self) -> Option<SymbolScopeType> {
        self.stack.last().map(|s| s.scope_type)
    }

    fn pop_scope(
        &mut self,
        expected_scope_type: SymbolScopeType,
    ) -> Option<Box<[(Identifier<'src>, IRSymbol<'src>)]>> {
        self.last_scope_type()
            .filter(|scope_type| *scope_type == expected_scope_type)
            .and_then(|_| self.stack.pop().map(|s| s.scope.into_boxed_slice()))
    }

    fn push_block(&mut self) {
        self.stack.push(SymbolScope::new(SymbolScopeType::Block));
    }

    fn pop_block(&mut self) -> Option<Box<[(Identifier<'src>, IRSymbol<'src>)]>> {
        self.pop_scope(SymbolScopeType::Block)
    }

    fn push_fn_frame(&mut self, args: impl Into<Box<[Identifier<'src>]>>) {
        let mut scope = SymbolScope::new(SymbolScopeType::Frame);
        args.into()
            .into_iter()
            .for_each(|name| scope.push(name, IRSymbol::Variable));
        self.stack.push(scope);
    }

    fn pop_fn_frame(&mut self) -> Option<Box<[(Identifier<'src>, IRSymbol<'src>)]>> {
        self.pop_scope(SymbolScopeType::Frame)
    }

    fn push(&mut self, name: Identifier<'src>, symbol: IRSymbol<'src>) {
        self.stack.last_mut().unwrap().push(name, symbol);
    }

    fn pop(&mut self) -> Option<(Identifier<'src>, IRSymbol<'src>)> {
        self.stack.last_mut().and_then(|v| v.pop())
    }
}

struct IRBuilder<'src> {
    func_table: Vec<Option<FuncInfo<'src>>>,
    symbol_stack: SymbolStack<'src>,
    scope_stack: Vec<Vec<Node<'src>>>,
    inside_loop: bool,
}

impl<'src> IRBuilder<'src> {
    fn new() -> Self {
        IRBuilder {
            func_table: Vec::new(),
            symbol_stack: SymbolStack::new(),
            scope_stack: Vec::new(),
            inside_loop: false,
        }
    }

    fn push_block(&mut self) {
        self.symbol_stack.push_block();
        self.scope_stack.push(Vec::new());
    }

    fn pop_block(&mut self) -> Vec<Node<'src>> {
        self.symbol_stack
            .pop_block()
            .unwrap()
            .into_iter()
            .for_each(|(_, symbol)| match symbol {
                IRSymbol::Function { index, info } => {
                    self.func_table.get_mut(index).unwrap().replace(info);
                }
                _ => {}
            });

        self.scope_stack.pop().unwrap()
    }

    fn push_node(&mut self, node: Node<'src>) {
        if !node.is_empty() {
            self.scope_stack.last_mut().unwrap().push(node)
        }
    }

    fn resolve_symbol(&self, name: Identifier<'src>) -> Result<'src, Reference> {
        let mut var_index = 0usize;
        for (symbol_name, symbol) in self.symbol_stack.iter() {
            if *symbol_name == name {
                return Ok(match symbol {
                    IRSymbol::Variable => Reference::Variable(var_index),
                    IRSymbol::Function { index, .. } => Reference::Function(*index),
                    IRSymbol::InaccesibleVariable => {
                        return Err(Box::new(FunctionCaptureError {
                            ident: name,
                            other_ident: *symbol_name,
                        }));
                    }
                });
            }
            if let IRSymbol::Variable = symbol {
                var_index += 1;
            }
        }
        Err(Box::new(UndefinedSymbolError { ident: name }))
    }
}

impl<'src> IR<'src> {
    pub fn function_table(&self) -> &Vec<FuncInfo<'src>> {
        &self.func_table
    }

    pub fn root(&self) -> &Node<'src> {
        &self.root
    }

    pub fn from_ast(ast: &Spanned<Expr<'src>>) -> Result<'src, IR<'src>> {
        let mut builder = IRBuilder::new();
        builder.push_block();
        let node = Self::from_ast_impl(ast, &mut builder)?;
        let scope = builder.pop_block();
        Ok(IR {
            func_table: builder.func_table.into_iter().map(Option::unwrap).collect(),
            root: Node::new(
                Tree::Scope {
                    nodes: if scope.is_empty() { vec![node] } else { scope },
                },
                ast.1,
            ),
        })
    }

    fn from_ast_impl(
        expr: &Spanned<Expr<'src>>,
        builder: &mut IRBuilder<'src>,
    ) -> Result<'src, Node<'src>> {
        Ok(match &expr.0 {
            Expr::Skip => Node::empty(expr.1),
            Expr::Block(expr) => {
                builder.push_block();
                let node = Self::from_ast_impl(expr.as_ref(), builder)?;
                let scope = builder.pop_block();
                Node::new(
                    Tree::Scope {
                        nodes: if scope.is_empty() { vec![node] } else { scope },
                    },
                    expr.1,
                )
            }
            Expr::Location { name, .. } => Node::new(
                Tree::Constant {
                    value: Value::LValue(builder.resolve_symbol(*name)?),
                },
                expr.1,
            ),
            Expr::Constant(value) => Node::new(
                Tree::Constant {
                    value: match value {
                        crate::value::Value::Number(x) => Value::RValue(RValue::Number(*x)),
                        _ => todo!(),
                    },
                },
                expr.1,
            ),
            Expr::Unary(_op, _arg) => todo!(),
            Expr::Binary(op, left, right) => {
                let func_ref = builder.resolve_symbol(op.map(|op| op.get_builtin_name()))?;
                Node::new(
                    Tree::Call {
                        callee: Box::new(Node::new(
                            Tree::Constant {
                                value: Value::LValue(func_ref),
                            },
                            op.1,
                        )),
                        args: [left, right]
                            .iter()
                            .map(|arg| Self::from_ast_impl(arg, builder))
                            .collect::<Result<_>>()?,
                    },
                    expr.1,
                )
            }
            Expr::Assign(lhs, rhs) => Node::new(
                Tree::Assign {
                    assignee: Box::new(Self::from_ast_impl(lhs.as_ref(), builder)?),
                    value: Box::new(Self::from_ast_impl(rhs.as_ref(), builder)?),
                },
                expr.1,
            ),
            Expr::Seq(first, second) => {
                let first_node = Self::from_ast_impl(first.as_ref(), builder)?;
                builder.push_node(first_node);
                let second_node = Self::from_ast_impl(second.as_ref(), builder)?;
                match &second.as_ref().0 {
                    Expr::Seq(..) => {}
                    _ => {
                        builder.push_node(second_node);
                    }
                }
                Node::empty(expr.1)
            }
            Expr::Let {
                var,
                is_mut: _,
                val,
            } => {
                let value_node = Self::from_ast_impl(val.as_ref(), builder)?;
                builder.symbol_stack.push(var.name, IRSymbol::Variable);
                Node::new(
                    Tree::Let {
                        variable: var.name.0,
                        value: Box::new(value_node),
                    },
                    expr.1,
                )
            }
            Expr::Function { name, func, .. } => {
                let func_symbol = IRSymbol::Function {
                    index: builder.func_table.len(),
                    info: FuncInfo {
                        name: *name,
                        args: func.args.clone(),
                        span: func.decl_span,
                        global: builder.scope_stack.len() <= 1,
                        body: None,
                    },
                };
                builder.func_table.push(None);
                builder.symbol_stack.push(*name, func_symbol);
                builder.symbol_stack.push_fn_frame(func.args.clone());

                let body_node = Self::from_ast_impl(&func.body, builder)?;

                builder.symbol_stack.pop_fn_frame();
                let mut func_symbol = builder.symbol_stack.pop().unwrap().1;
                if let IRSymbol::Function { ref mut info, .. } = func_symbol {
                    info.body = Some(body_node);
                } else {
                    unreachable!()
                }
                builder.symbol_stack.push(*name, func_symbol);

                Node::empty(expr.1)
            }
            Expr::If {
                cond,
                if_true,
                if_false,
            } => Node::new(
                Tree::If {
                    condition: Box::new(Self::from_ast_impl(cond.as_ref(), builder)?),
                    if_then: Box::new(Self::from_ast_impl(if_true.as_ref(), builder)?),
                    if_else: Box::new(
                        if_false
                            .as_ref()
                            .map(|if_false| Self::from_ast_impl(if_false.as_ref(), builder))
                            .unwrap_or_else(|| Ok(Node::empty(expr.1)))?,
                    ),
                },
                expr.1,
            ),
            Expr::Return(expr) => Node::new(
                Tree::Return {
                    value: Box::new(Self::from_ast_impl(expr.as_ref(), builder)?),
                },
                expr.1,
            ),
            Expr::While { cond, body } => {
                let cond_node = Self::from_ast_impl(cond.as_ref(), builder)?;
                let prev_inside_loop = builder.inside_loop;
                builder.inside_loop = true;
                let body_node = Self::from_ast_impl(body.as_ref(), builder)?;
                builder.inside_loop = prev_inside_loop;
                Node::new(
                    Tree::While {
                        condition: Box::new(cond_node),
                        body: Box::new(body_node),
                    },
                    expr.1,
                )
            }
            Expr::Call(callee, args) => {
                let prev_inside_loop = builder.inside_loop;
                builder.inside_loop = false;
                let callee_node = Self::from_ast_impl(callee.as_ref(), builder)?;
                builder.inside_loop = prev_inside_loop;
                Node::new(
                    Tree::Call {
                        callee: Box::new(callee_node),
                        args: args
                            .iter()
                            .map(|arg| Self::from_ast_impl(arg, builder))
                            .collect::<Result<_>>()?,
                    },
                    expr.1,
                )
            }
            Expr::Break(keyword) => {
                if !builder.inside_loop {
                    return Err(Box::new(BreakOutsideOfLoopError {
                        keyword: keyword.clone(),
                    }));
                }
                Node::new(Tree::Break, expr.1)
            }
            Expr::Continue(keyword) => {
                if !builder.inside_loop {
                    return Err(Box::new(BreakOutsideOfLoopError {
                        keyword: keyword.clone(),
                    }));
                }
                Node::new(Tree::Continue, expr.1)
            }
            _ => todo!(),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Node<'src> {
    span: SimpleSpan,
    tree: Tree<'src>,
}

impl<'src> Node<'src> {
    fn new(tree: Tree<'src>, span: SimpleSpan) -> Self {
        Self { span, tree }
    }

    fn empty(span: SimpleSpan) -> Self {
        Self {
            span: span.to_end(),
            tree: Tree::Scope { nodes: Vec::new() },
        }
    }

    pub fn span(&self) -> SimpleSpan {
        self.span
    }

    pub fn tree(&self) -> &Tree<'src> {
        &self.tree
    }

    pub fn is_empty(&self) -> bool {
        match &self.tree {
            Tree::Scope { nodes } => nodes.is_empty(),
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Tree<'src> {
    Constant {
        value: Value,
    },

    Scope {
        nodes: Vec<Node<'src>>,
    },

    Call {
        callee: Box<Node<'src>>,
        args: Vec<Node<'src>>,
    },

    Assign {
        assignee: Box<Node<'src>>,
        value: Box<Node<'src>>,
    },

    Let {
        variable: &'src str,
        value: Box<Node<'src>>,
    },

    If {
        condition: Box<Node<'src>>,
        if_then: Box<Node<'src>>,
        if_else: Box<Node<'src>>,
    },

    While {
        condition: Box<Node<'src>>,
        body: Box<Node<'src>>,
    },

    Return {
        value: Box<Node<'src>>,
    },

    Break,

    Continue,
}
