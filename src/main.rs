use std::borrow::Borrow;

use chumsky::prelude::*;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    Dereference,
}

impl UnaryOp {
    fn parse() -> impl Parser<char, UnaryOp, Error = Simple<char>> + Clone {
        choice((
            just("-").to(UnaryOp::Negate),
            just("*").to(UnaryOp::Dereference),
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub enum BinaryOpPriority {
    Add,
    Multiply,
}

impl BinaryOp {
    fn parse(
        priority: BinaryOpPriority,
    ) -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        match priority {
            BinaryOpPriority::Add => choice((
                just("+").to(BinaryOp::Add),
                just("-").to(BinaryOp::Subtract),
            )),
            BinaryOpPriority::Multiply => choice((
                just("*").to(BinaryOp::Multiply),
                just("/").to(BinaryOp::Divide),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(i32),
    Reference(usize),
    Void,
}

impl Value {
    fn unwrap_number(self) -> i32 {
        match self {
            Value::Number(val) => val,
            _ => panic!("unwrap of non-number value {:?}", self),
        }
    }

    fn unwrap_reference(self) -> usize {
        match self {
            Value::Reference(i) => i,
            _ => panic!("unwrap of non-reference value {:?}", self),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Skip,                                   // Void
    Ignore(Box<Expr>),                      // Void
    Variable(String),                       // Value
    Constant(i32),                          // Value
    Reference(String),                      // Value
    Unary(UnaryOp, Box<Expr>),              // Value
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // Value
    Assign(Box<Expr>, Box<Expr>),           // Void
    Seq(Box<Expr>, Box<Expr>),              // any
    Let(String, Box<Expr>),                 // Void
    Scope(String, Box<Expr>, Box<Expr>),    // any
    If(Box<Expr>, Box<Expr>, Box<Expr>),    // any
}

#[derive(Default)]
struct Stack(Vec<(String, Value)>);

impl Stack {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index).map(|e| &e.1)
    }

    fn find<Q>(&self, k: &Q) -> Option<&Value>
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0
            .iter()
            .rev()
            .find(|(var, _)| var.borrow() == k)
            .map(|e| &e.1)
    }

    fn index<Q>(&self, k: &Q) -> Option<usize>
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0.iter().position(|(var, _)| var.borrow() == k)
    }

    fn assign(&mut self, index: usize, v: Value) {
        self.0.get_mut(index).map(|e| e.1 = v);
    }

    fn push(&mut self, k: String, v: Value) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(String, Value)> {
        self.0.pop()
    }
}

impl Expr {
    fn new_seq(first_expr: Expr, second_expr: Expr) -> Expr {
        match (first_expr, second_expr) {
            (Expr::Let(var, val_expr), inner_expr) => {
                Expr::Scope(var, val_expr, Box::new(inner_expr))
            }
            (Expr::Skip, second_expr) => second_expr,
            (first_expr, second_expr) => Expr::Seq(
                if first_expr.is_value() {
                    Box::new(Expr::Ignore(Box::new(first_expr)))
                } else {
                    Box::new(first_expr)
                },
                Box::new(second_expr),
            ),
        }
    }

    fn is_value(&self) -> bool {
        // TODO: maybe optimize
        match self {
            Expr::Skip => false,
            Expr::Ignore(_) => false,
            Expr::Variable(_) => true,
            Expr::Constant(_) => true,
            Expr::Reference(_) => true,
            Expr::Unary(_, _) => true,
            Expr::Binary(_, _, _) => true,
            Expr::Assign(_, _) => false,
            Expr::Seq(_, second_expr) => second_expr.is_value(),
            Expr::Let(_, _) => false,
            Expr::Scope(_, _, cont_expr) => cont_expr.is_value(),
            Expr::If(_, _, false_expr) => false_expr.is_value(),
        }
    }

    fn eval_impl(&self, stack: &mut Stack) -> Value {
        match self {
            Expr::Skip => Value::Void,
            Expr::Ignore(expr) => {
                expr.eval_impl(stack);
                Value::Void
            }
            Expr::Variable(var) => stack.find(var).cloned().unwrap_or(Value::Void),
            Expr::Constant(val) => Value::Number(val.clone()),
            Expr::Reference(var) => stack
                .index(var)
                .map(|i| Value::Reference(i))
                .unwrap_or(Value::Void),
            Expr::Unary(op, arg_expr) => {
                let arg = arg_expr.eval_impl(stack);
                match op {
                    UnaryOp::Negate => Value::Number(arg.unwrap_number().wrapping_neg()),
                    UnaryOp::Dereference => match arg {
                        Value::Reference(i) => stack.get(i).cloned().unwrap_or(Value::Void),
                        _ => Value::Void,
                    },
                }
            }
            Expr::Binary(op, left_expr, right_expr) => {
                let left = left_expr.eval_impl(stack).unwrap_number();
                let right = right_expr.eval_impl(stack).unwrap_number();
                Value::Number(match op {
                    BinaryOp::Add => left.wrapping_add(right),
                    BinaryOp::Subtract => left.wrapping_sub(right),
                    BinaryOp::Multiply => left.wrapping_mul(right),
                    BinaryOp::Divide => left.wrapping_div(right),
                })
            }
            Expr::Assign(lhs_expr, rhs_expr) => {
                let lhs = lhs_expr.eval_impl(stack).unwrap_reference();
                let rhs = rhs_expr.eval_impl(stack);
                stack.assign(lhs, rhs.clone());
                rhs
            }
            Expr::Seq(first_expr, second_expr) => {
                first_expr.eval_impl(stack);
                second_expr.eval_impl(stack)
            }
            Expr::Let(_, _) => panic!("Let is an intermediate node and should not be present in final AST"),
            Expr::Scope(var, val_expr, cont_expr) => {
                let val = val_expr.eval_impl(stack);
                stack.push(var.clone(), val);
                let res = cont_expr.eval_impl(stack);
                stack.pop();
                res
            }
            Expr::If(cond_expr, true_expr, false_expr) => {
                let cond = cond_expr.eval_impl(stack).unwrap_number();
                if cond != 0 {
                    true_expr.eval_impl(stack)
                } else {
                    false_expr.eval_impl(stack)
                }
            }
        }
    }

    pub fn eval(&self) -> Value {
        let mut stack: Stack = Stack::new();
        self.eval_impl(&mut stack)
    }
}

fn stmt() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|stmt| {
        let token = |c| just(c).padded();

        let var = text::ident().padded();

        let num = text::int(10)
            .map(|s: String| Expr::Constant(s.parse().unwrap()))
            .padded();

        let block = stmt
            .clone()
            .delimited_by(just("{"), just("}"))
            .map(|opt: Option<Expr>| opt.unwrap_or(Expr::Skip))
            .padded();

        let if_expr_cons = |expr| {
            recursive(|if_stmt| {
                token("if")
                    .then(expr)
                    .then(block.clone())
                    .then(token("else").then(if_stmt.or(block.clone())).or_not())
                    .map(|(((_, cond), if_true), if_false)| {
                        Expr::If(
                            Box::new(cond),
                            Box::new(if_true),
                            Box::new(if_false.map(|x| x.1).unwrap_or(Expr::Skip)),
                        )
                    })
            })
        };

        let expr = recursive(|expr| {
            let atom = choice((
                if_expr_cons(expr.clone()),
                expr.clone().delimited_by(just('('), just(')')),
                block.clone(),
                var.clone().map(|var| Expr::Variable(var)),
                token("&").then(var).map(|(_, var)| Expr::Reference(var)),
                num,
            ))
            .padded();

            let unary = UnaryOp::parse()
                .padded()
                .repeated()
                .then(atom)
                .foldr(|op, x| Expr::Unary(op, Box::new(x)));

            let product = unary
                .clone()
                .then(
                    BinaryOp::parse(BinaryOpPriority::Multiply)
                        .padded()
                        .then(unary)
                        .repeated(),
                )
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            let sum = product
                .clone()
                .then(
                    BinaryOp::parse(BinaryOpPriority::Add)
                        .then(product)
                        .repeated(),
                )
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            sum
        });

        let let_expr = token("let")
            .then(var)
            .then(token("=").then(expr.clone()).or_not())
            .map(|((_, var), rhs)| {
                Expr::Let(var, Box::new(rhs.map(|x| x.1).unwrap_or(Expr::Constant(0))))
            });

        let assign = var
            .clone()
            .map(|var| Expr::Reference(var))
            .or(token("*").then(expr.clone()).map(|x| x.1))
            .then(token("=").then(expr.clone()).map(|(_, e)| e))
            .map(|(lhs, rhs)| Expr::Assign(Box::new(lhs), Box::new(rhs)));

        let atom = choice((let_expr, assign, expr.clone())).padded();

        block
            .clone()
            .or(if_expr_cons(expr.clone()))
            .then(stmt.clone())
            .map(|(first_expr, second_opt)| {
                Some(match second_opt {
                    Some(second_expr) => Expr::new_seq(first_expr, second_expr),
                    None => first_expr,
                })
            })
            .or(atom
                .clone()
                .or_not()
                .then(token(";").then(stmt.clone()).or_not())
                .map(|(first_opt, second_opt)| match second_opt {
                    Some((_, second_opt)) => Some(Expr::new_seq(
                        first_opt.unwrap_or(Expr::Skip),
                        second_opt.unwrap_or(Expr::Skip),
                    )),
                    None => first_opt,
                }))
            .padded()
    })
    .map(|opt: Option<Expr>| opt.unwrap_or(Expr::Skip))
    .then_ignore(end())
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match stmt().parse(src) {
        Ok(ast) => {
            println!("{:?}", ast);
            println!("{:?}", ast.eval())
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
