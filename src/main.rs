use std::borrow::Borrow;

use chumsky::prelude::*;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
}

impl Into<UnaryOp> for &str {
    fn into(self) -> UnaryOp {
        match self {
            "-" => UnaryOp::Neg,
            c => panic!("unknown unary operator {}", c),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Value {
    Number(i32),
    Reference(String),
    Void,
}

impl Value {
    fn unwrap_number(self) -> i32 {
        match self {
            Value::Number(val) => val,
            _ => panic!("unwrap of non-number value {:?}", self),
        }
    }

    fn unwrap_reference(self) -> String {
        match self {
            Value::Reference(var) => var,
            _ => panic!("unwrap of non-reference value {:?}", self),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Skip,
    Ignore(Box<Expr>),
    Variable(String),
    Constant(i32),
    Reference(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Let(String),
    Scope(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Default)]
struct Stack(Vec<(String, i32)>);

impl Stack {
    fn new() -> Self {
        Self::default()
    }

    fn get<Q>(&self, k: &Q) -> Option<i32>
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0
            .iter()
            .rev()
            .find(|(var, _)| var.borrow() == k)
            .map(|e| e.1)
    }

    fn push(&mut self, k: String, v: i32) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(String, i32)> {
        self.0.pop()
    }

    fn assign<Q>(&mut self, k: &Q, v: i32)
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0
            .iter_mut()
            .rev()
            .find(|(var, _)| var.borrow() == k)
            .map(|kv| kv.1 = v);
    }
}

impl Expr {
    fn eval_impl(&self, stack: &mut Stack) -> Value {
        match self {
            Expr::Skip => Value::Void,
            Expr::Ignore(expr) => {
                expr.eval_impl(stack);
                Value::Void
            }
            Expr::Variable(var) => Value::Number(stack.get(var).unwrap()),
            Expr::Constant(val) => Value::Number(val.clone()),
            Expr::Reference(var) => Value::Reference(var.clone()),
            Expr::Unary(op, arg_expr) => {
                let arg = arg_expr.eval_impl(stack).unwrap_number();
                Value::Number(match op {
                    UnaryOp::Neg => arg.wrapping_neg(),
                })
            }
            Expr::Binary(op, left_expr, right_expr) => {
                let left = left_expr.eval_impl(stack).unwrap_number();
                let right = right_expr.eval_impl(stack).unwrap_number();
                Value::Number(match op {
                    BinaryOp::Add => left.wrapping_add(right),
                    BinaryOp::Sub => left.wrapping_sub(right),
                    BinaryOp::Mul => left.wrapping_mul(right),
                    BinaryOp::Div => left.wrapping_div(right),
                })
            }
            Expr::Assign(lhs_expr, rhs_expr) => {
                let lhs = lhs_expr.eval_impl(stack).unwrap_reference();
                let rhs = rhs_expr.eval_impl(stack).unwrap_number();
                stack.assign(&lhs, rhs);
                Value::Number(rhs)
            }
            Expr::Seq(first_expr, second_expr) => {
                first_expr.eval_impl(stack);
                second_expr.eval_impl(stack)
            }
            Expr::Let(_) => panic!("let is an intermediate node and should be absent"),
            Expr::Scope(var, expr) => {
                stack.push(var.clone(), 0);
                let res = expr.eval_impl(stack);
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

fn expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let token = |c| just(c).padded();

        let var = text::ident().padded();

        let num = text::int(10)
            .map(|s: String| Expr::Constant(s.parse().unwrap()))
            .padded();

        let block = expr.clone().delimited_by(just("{"), just("}"));

        let let_expr = token("let").then(var).map(|(_, var)| Expr::Let(var));

        let if_expr = recursive(|if_expr| {
            token("if")
                .then(expr.clone())
                .then(block.clone())
                .then(token("else").then(if_expr.or(block.clone())).or_not())
                .map(|(((_, cond), if_true), if_false)| {
                    Expr::If(
                        Box::new(cond),
                        Box::new(if_true),
                        Box::new(if_false.map(|x| x.1).unwrap_or(Expr::Skip)),
                    )
                })
        });

        let atom = choice((
            if_expr,
            expr.clone().delimited_by(just('('), just(')')),
            expr.clone().delimited_by(just('{'), just('}')),
            var.map(|var| Expr::Variable(var)),
            num,
        ))
        .padded();

        let unary = token("-")
            .repeated()
            .then(atom)
            .foldr(|op, x| Expr::Unary(op.into(), Box::new(x)));

        let product = unary
            .clone()
            .then(
                choice((token("*").to(BinaryOp::Mul), token("/").to(BinaryOp::Div)))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                choice((token("+").to(BinaryOp::Add), token("-").to(BinaryOp::Sub)))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        let assign = sum
            .clone()
            .then(token("=").then(sum).map(|(_, e)| e).repeated())
            .map(|(a, mut b): (Expr, Vec<Expr>)| {
                b.insert(0, a);
                let c = b.pop().unwrap();
                (b, c)
            })
            .foldr(|lhs, rhs| {
                Expr::Assign(
                    Box::new(match lhs {
                        Expr::Variable(var) => Expr::Reference(var),
                        _ => lhs,
                    }),
                    Box::new(rhs),
                )
            });

        let stmt = choice((let_expr, assign)).padded();

        stmt.clone()
            .then(token(";").then(stmt.or_not()).map(|(_, e)| e.unwrap_or(Expr::Skip)).repeated())
            .map(|(a, mut b)| {
                b.insert(0, a);
                let c = b.pop().unwrap();
                (b, c)
            })
            .foldr(|first, second| match first {
                Expr::Let(var) => Expr::Scope(var, Box::new(second)),
                _ => Expr::Seq(Box::new(first), Box::new(second)),
            })
    })
    .then_ignore(end())
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match expr().parse(src) {
        Ok(ast) => {
            println!("{:?}", ast);
            println!("{:?}", ast.eval())
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
