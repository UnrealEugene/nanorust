use core::cell::Cell;

use crate::expr::*;
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use chumsky::prelude::*;

type ParseError<'src> = extra::Err<Rich<'src, char>>;

impl UnaryOp {
    fn parse<'src>() -> impl Parser<'src, &'src str, UnaryOp, ParseError<'src>> + Copy + Clone {
        choice((
            just("-").to(UnaryOp::Negate),
            just("!").to(UnaryOp::LogicNot),
            just("&").to(UnaryOp::Reference),
            just("*").to(UnaryOp::Dereference),
        ))
    }
}

impl BinaryOp {
    fn parse_sum<'src>() -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone
    {
        choice((
            just("+").to(BinaryOp::Add),
            just("-").to(BinaryOp::Subtract),
        ))
    }

    fn parse_product<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        choice((
            just("*").to(BinaryOp::Multiply),
            just("/").to(BinaryOp::Divide),
            just("%").to(BinaryOp::Modulo),
        ))
    }

    fn parse_compare<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        choice((
            just("==").to(BinaryOp::Equal),
            just("!=").to(BinaryOp::NotEqual),
            just("<=").to(BinaryOp::LessEqual),
            just(">=").to(BinaryOp::GreaterEqual),
            just("<").to(BinaryOp::Less),
            just(">").to(BinaryOp::Greater),
        ))
    }

    fn parse_logic_and<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        just("&&").to(BinaryOp::LogicAnd)
    }

    fn parse_logic_or<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        just("||").to(BinaryOp::LogicOr)
    }

    fn parse_range<'src>() -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone
    {
        choice((
            just("..=").to(BinaryOp::RangeInclusive),
            just("..").to(BinaryOp::Range),
        ))
    }
}

pub fn parse_stmt<'src>() -> impl Parser<'src, &'src str, Expr<'src>, ParseError<'src>> {
    recursive(|stmt| {
        let token = |c: &'static str| just(c).padded();

        let keyword = |s: &'static str| text::keyword(s).padded();

        let var = text::ident::<'src, &'src str, _>().padded();

        let num = text::int(10)
            .map(|s: &'src str| s.parse::<i32>().unwrap())
            .padded()
            .boxed();

        let block = stmt
            .clone()
            .delimited_by(just("{"), just("}"))
            .map(|opt: Option<Expr>| opt.unwrap_or(Expr::Skip))
            .padded()
            .boxed();

        let if_expr_cons = |expr| {
            recursive(|if_stmt| {
                keyword("if")
                    .ignore_then(expr)
                    .then(block.clone())
                    .then(
                        keyword("else")
                            .ignore_then(if_stmt.or(block.clone()))
                            .or_not(),
                    )
                    .map(|((cond, if_true), if_false)| {
                        Expr::If(
                            Box::new(cond),
                            Box::new(if_true),
                            Box::new(if_false.unwrap_or(Expr::Skip)),
                        )
                    })
            })
        };

        let var_list = var
            .clone()
            .separated_by(just(","))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded()
            .boxed();

        let expr = recursive(|expr| {
            let lambda = token("|")
                .ignore_then(var_list.clone())
                .then_ignore(token("|"))
                .then(expr.clone())
                .map(|(var_names, body_expr)| {
                    Expr::Closure(Rc::new(Function(var_names, Box::new(body_expr))))
                })
                .padded()
                .boxed();

            let return_expr = keyword("return")
                .ignore_then(expr.clone())
                .map(|val| Expr::Return(Box::new(val)))
                .boxed();

            let atom = choice((
                expr.clone().delimited_by(just('('), just(')')),
                keyword("true").map(|_| Expr::Constant(Value::Boolean(true))),
                keyword("false").map(|_| Expr::Constant(Value::Boolean(false))),
                var.clone()
                    .map(|var| Expr::Location(Cell::new(Pointer::Invalid), var)),
                num.map(|x| Expr::Constant(Value::Number(x))),
            ))
            .padded()
            .boxed();

            let expr_list = expr
                .clone()
                .separated_by(just(","))
                .allow_trailing()
                .collect::<Vec<_>>()
                .padded()
                .boxed();

            let call = atom
                .clone()
                .foldl(
                    expr_list
                        .clone()
                        .delimited_by(just("("), just(")"))
                        .padded()
                        .repeated(),
                    |fn_expr, arg_list| Expr::Call(Box::new(fn_expr), arg_list),
                )
                .boxed();

            let unary = UnaryOp::parse()
                .padded()
                .repeated()
                .foldr(call.clone(), |op, x| Expr::Unary(op, Box::new(x)))
                .boxed();

            let product = unary
                .clone()
                .foldl(
                    BinaryOp::parse_product().padded().then(unary).repeated(),
                    |lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            let sum = product
                .clone()
                .foldl(
                    BinaryOp::parse_sum().padded().then(product).repeated(),
                    |lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            let compare = sum
                .clone()
                .then(BinaryOp::parse_compare().padded().then(sum).or_not())
                .map(|(lhs, rhs_opt)| match rhs_opt {
                    Some((op, rhs)) => Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                    None => lhs,
                })
                .boxed();

            let logic_and = compare
                .clone()
                .foldl(
                    BinaryOp::parse_logic_and()
                        .padded()
                        .then(compare)
                        .repeated(),
                    |lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            let logic_or = logic_and
                .clone()
                .foldl(
                    BinaryOp::parse_logic_or()
                        .padded()
                        .then(logic_and)
                        .repeated(),
                    |lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                )
                .boxed();

            let range = logic_or
                .clone()
                .then(BinaryOp::parse_range().then(logic_or).or_not())
                .map(|(lhs, rhs_opt)| match rhs_opt {
                    Some((op, rhs)) => Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                    None => lhs,
                })
                .boxed();

            choice((
                if_expr_cons(expr.clone()),
                return_expr,
                keyword("continue").map(|_| Expr::Continue),
                keyword("break").map(|_| Expr::Break),
                lambda.clone(),
                block.clone(),
                range.clone(),
            ))
            .padded()
            .boxed()
        });

        let let_expr = keyword("let")
            .ignore_then(var.clone())
            .then(token("=").ignore_then(expr.clone()).or_not())
            .map(|(var, rhs)| Expr::Let(var, Box::new(rhs.unwrap_or(Expr::Skip))))
            .boxed();

        let assign = var
            .clone()
            .map(|var| {
                Expr::Unary(
                    UnaryOp::Reference,
                    Box::new(Expr::Location(Cell::new(Pointer::Invalid), var)),
                )
            })
            .or(token("*").ignore_then(expr.clone()))
            .then(token("=").ignore_then(expr.clone()))
            .map(|(lhs, rhs)| Expr::Assign(Box::new(lhs), Box::new(rhs)))
            .boxed();

        let while_stmt = keyword("while")
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond_expr, inner_expr)| {
                Expr::While(Box::new(cond_expr), Box::new(Expr::new_ignore(inner_expr)))
            })
            .boxed();

        let for_stmt = keyword("for")
            .ignore_then(var.clone())
            .then_ignore(keyword("in"))
            .then(expr.clone())
            .then(block.clone())
            .map(|((var, iter_expr), inner_expr)| {
                Expr::For(var, Box::new(iter_expr), Box::new(inner_expr))
            })
            .boxed();

        let func_stmt = keyword("fn")
            .ignore_then(var.clone())
            .then(var_list.clone().delimited_by(just("("), just(")")))
            .then(block.clone())
            .map(|((name, var_names), body_expr)| {
                Expr::Function(name, Rc::new(Function(var_names, Box::new(body_expr))))
            })
            .boxed();

        let atom = choice((let_expr, assign, expr.clone())).padded().boxed();

        block
            .clone()
            .or(if_expr_cons(expr.clone()))
            .or(while_stmt.clone())
            .or(for_stmt.clone())
            .or(func_stmt.clone())
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
                .then(token(";").ignore_then(stmt.clone()).or_not())
                .map(|(first_opt, second_opt)| match second_opt {
                    Some(second_opt) => Some(Expr::new_seq(
                        first_opt.unwrap_or(Expr::Skip),
                        second_opt.unwrap_or(Expr::Skip),
                    )),
                    None => first_opt,
                }))
            .map(|expr_opt| match expr_opt {
                Some(Expr::Let(var, val_expr)) => {
                    Some(Expr::VarScope(var, val_expr, Box::new(Expr::Skip)))
                }
                Some(Expr::Function(name, func)) => {
                    Some(Expr::FunScope(name, func, Box::new(Expr::Skip)))
                }
                _ => expr_opt,
            })
            .padded()
    })
    .map(|opt: Option<Expr>| opt.unwrap_or(Expr::Skip))
    .then_ignore(end())
}
