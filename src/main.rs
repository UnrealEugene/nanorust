use chumsky::prelude::*;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
}

impl Into<UnaryOp> for char {
    fn into(self) -> UnaryOp {
        match self {
            '-' => UnaryOp::Neg,
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
pub enum Expr {
    Var(i32),
    Num(i32),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn eval(&self) -> i32 {
        match self {
            Expr::Var(_) => todo!(),
            Expr::Num(x) => x.clone(),
            Expr::Unary(op, arg_expr) => {
                let arg = arg_expr.eval();
                match op {
                    UnaryOp::Neg => arg.wrapping_neg(),
                }
            },
            Expr::Binary(op, left_expr, right_expr) => {
                let left = left_expr.eval();
                let right = right_expr.eval();
                match op {
                    BinaryOp::Add => left.wrapping_add(right),
                    BinaryOp::Sub => left.wrapping_sub(right),
                    BinaryOp::Mul => left.wrapping_mul(right),
                    BinaryOp::Div => left.wrapping_div(right),
                }
            },
        }
    }
}

fn expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let num = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let atom = num.or(expr.delimited_by(just('('), just(')'))).padded();

        let token = |c| just(c).padded();

        let unary = token('-')
            .repeated()
            .then(atom)
            .foldr(|op, x| Expr::Unary(op.into(), Box::new(x)));

        let product = unary
            .clone()
            .then(
                choice((token('*').to(BinaryOp::Mul), token('/').to(BinaryOp::Div)))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                choice((token('+').to(BinaryOp::Add), token('-').to(BinaryOp::Sub)))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        sum
    })
    .then_ignore(end())
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match expr().parse(src) {
        Ok(ast) => {
            println!("{:?}", ast);
            println!("{}", ast.eval())
        },
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
