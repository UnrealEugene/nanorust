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
pub enum ValueType {
    Data,
    Reference,
    Void,
}

#[derive(Debug, Clone)]
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
    Skip,                                   // Void
    Ignore(Box<Expr>),                      // Void
    Variable(String),                       // any
    Constant(i32),                          // Data
    Reference(String),                      // Reference
    Dereference(Box<Expr>),                 // any
    Unary(UnaryOp, Box<Expr>),              // Data
    Binary(BinaryOp, Box<Expr>, Box<Expr>), // Data
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

    fn get<Q>(&self, k: &Q) -> Option<Value>
    where
        String: Borrow<Q>,
        Q: Eq + ?Sized,
    {
        self.0
            .iter()
            .rev()
            .find(|(var, _)| var.borrow() == k)
            .map(|e| e.1.clone())
    }

    fn push(&mut self, k: String, v: Value) {
        self.0.push((k, v));
    }

    fn pop(&mut self) -> Option<(String, Value)> {
        self.0.pop()
    }

    fn assign<Q>(&mut self, k: &Q, v: Value)
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
            Expr::Variable(var) => stack.get(var).unwrap_or(Value::Void),
            Expr::Constant(val) => Value::Number(val.clone()),
            Expr::Reference(var) => Value::Reference(var.clone()),
            Expr::Dereference(expr) => match expr.eval_impl(stack) {
                Value::Reference(var) => stack.get(&var).unwrap_or(Value::Void),
                _ => Value::Void,
            },
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
                let rhs = rhs_expr.eval_impl(stack);
                stack.assign(&lhs, rhs.clone());
                rhs
            }
            Expr::Seq(first_expr, second_expr) => {
                first_expr.eval_impl(stack);
                second_expr.eval_impl(stack)
            }
            Expr::Let(_, _) => panic!("let is an intermediate node and should be absent"),
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

        let mut x = 1;

        *&mut x = 1;

        let block = stmt.clone().delimited_by(just("{"), just("}"));

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
                token("&")
                    .then(var.clone())
                    .map(|(_, var)| Expr::Reference(var)),
                token("*")
                    .then(expr.clone())
                    .map(|(_, expr)| Expr::Dereference(Box::new(expr))),
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
            .map(|(a, b)| Expr::Seq(Box::new(a), Box::new(b)))
            .or(atom
                .clone()
                .or_not()
                .then(token(";").then(stmt.clone()).or_not())
                .map(|(first_opt, second_opt)| {
                    let first_expr = first_opt.unwrap_or(Expr::Skip);
                    let second_expr = second_opt.map(|x| x.1).unwrap_or(Expr::Skip);
                    match (first_expr, second_expr) {
                        (Expr::Let(var, val_expr), inner_expr) => {
                            Expr::Scope(var, val_expr, Box::new(inner_expr))
                        }
                        (Expr::Skip, second_expr) => second_expr,
                        (first_expr, Expr::Skip) => first_expr,
                        (first_expr, second_expr) => {
                            Expr::Seq(Box::new(first_expr), Box::new(second_expr))
                        }
                    }
                }))
    })
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
