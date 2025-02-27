use core::cell::Cell;

use crate::expr::*;
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use chumsky::{input::MapExtra, prelude::*};

type ParseError<'src> = extra::Err<Rich<'src, char>>;

impl UnaryOp {
    fn parse<'src>() -> impl Parser<'src, &'src str, UnaryOp, ParseError<'src>> + Copy + Clone {
        choice((
            just("-").to(UnaryOp::Negate),
            just("!").to(UnaryOp::LogicNot),
            just("&").to(UnaryOp::Reference),
            just("*").to(UnaryOp::Dereference),
        ))
        .labelled("unary operator")
    }
}

impl BinaryOp {
    fn parse_sum<'src>() -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone
    {
        choice((
            just("+").to(BinaryOp::Add),
            just("-").to(BinaryOp::Subtract),
        ))
        .labelled("binary operator")
    }

    fn parse_product<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        choice((
            just("*").to(BinaryOp::Multiply),
            just("/").to(BinaryOp::Divide),
            just("%").to(BinaryOp::Modulo),
        ))
        .labelled("binary operator")
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
        .labelled("binary operator")
    }

    fn parse_logic_and<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        just("&&")
            .to(BinaryOp::LogicAnd)
            .labelled("binary operator")
    }

    fn parse_logic_or<'src>(
    ) -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone {
        just("||").to(BinaryOp::LogicOr).labelled("binary operator")
    }

    fn parse_range<'src>() -> impl Parser<'src, &'src str, BinaryOp, ParseError<'src>> + Copy + Clone
    {
        choice((
            just("..=").to(BinaryOp::RangeInclusive),
            just("..").to(BinaryOp::Range),
        ))
        .labelled("binary operator")
    }
}

fn span_wrap<'src, T>(x: T, e: &mut MapExtra<'src, '_, &'src str, ParseError<'src>>) -> Spanned<T> {
    Spanned(x, e.span())
}

pub fn parse_stmt<'src>() -> impl Parser<'src, &'src str, Spanned<Expr<'src>>, ParseError<'src>> {
    recursive(|stmt| {
        let token = |c: &'static str| just(c).padded();

        let keyword = |s: &'static str| text::keyword(s).padded();

        let var = text::ident::<'src, &'src str, _>()
            .map_with(span_wrap)
            .padded();

        let num = text::int(10).from_str().unwrapped().labelled("integer").boxed();

        let block = stmt
            .clone()
            .delimited_by(just("{"), just("}"))
            .map(|opt: Spanned<Option<Expr>>| {
                Expr::Block(Box::new(opt.map(Option::unwrap_or_default)))
            })
            .map_with(span_wrap)
            .labelled("block")
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
                    .map_with(|((cond, if_true), if_false), e| {
                        Expr::If(
                            Box::new(cond),
                            Box::new(if_true),
                            Box::new(Spanned::unwrap_or_default(if_false, e.span())),
                        )
                    })
                    .map_with(span_wrap)
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
            let lambda = var_list
                .clone()
                .delimited_by(just("|"), just("|"))
                .then(expr.clone())
                .map(|(var_names, body_expr)| {
                    Expr::Closure(Rc::new(Function(var_names, Box::new(body_expr))))
                })
                .map_with(span_wrap)
                .padded()
                .boxed();

            let return_expr = keyword("return")
                .ignore_then(expr.clone().or_not())
                .map_with(|opt, e| {
                    Expr::Return(Box::new(Spanned::unwrap_or_default(opt, e.span())))
                })
                .map_with(span_wrap)
                .boxed();

            let atom = choice((
                expr.clone().delimited_by(just('('), just(')')).map(|s| s.0),
                keyword("true").map(|_| Expr::Constant(Value::Boolean(true))),
                keyword("false").map(|_| Expr::Constant(Value::Boolean(false))),
                var.clone()
                    .map(|var| Expr::Location(Cell::new(Pointer::Invalid), var.0)),
                num.map(|x| Expr::Constant(Value::Number(x))),
            ))
            .map_with(span_wrap)
            .labelled("expression")
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
                .foldl_with(
                    expr_list
                        .clone()
                        .delimited_by(just("(").labelled("function call"), just(")"))
                        .padded()
                        .repeated(),
                    |fn_expr, arg_list, e| {
                        Spanned(Expr::Call(Box::new(fn_expr), arg_list), e.span())
                    },
                )
                .boxed();

            let unary = UnaryOp::parse()
                .labelled("expression")
                .padded()
                .repeated()
                .foldr_with(call.clone(), |op, x, e| {
                    Spanned(Expr::Unary(op, Box::new(x)), e.span())
                })
                .boxed();

            let product = unary
                .clone()
                .foldl_with(
                    BinaryOp::parse_product().padded().then(unary).repeated(),
                    |lhs, (op, rhs), e| {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    },
                )
                .boxed();

            let sum = product
                .clone()
                .foldl_with(
                    BinaryOp::parse_sum().padded().then(product).repeated(),
                    |lhs, (op, rhs), e| {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    },
                )
                .boxed();

            let compare = sum
                .clone()
                .then(BinaryOp::parse_compare().padded().then(sum).or_not())
                .map_with(|(lhs, rhs_opt), e| match rhs_opt {
                    Some((op, rhs)) => {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    }
                    None => lhs,
                })
                .boxed();

            let logic_and = compare
                .clone()
                .foldl_with(
                    BinaryOp::parse_logic_and()
                        .padded()
                        .then(compare)
                        .repeated(),
                    |lhs, (op, rhs), e| {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    },
                )
                .boxed();

            let logic_or = logic_and
                .clone()
                .foldl_with(
                    BinaryOp::parse_logic_or()
                        .padded()
                        .then(logic_and)
                        .repeated(),
                    |lhs, (op, rhs), e| {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    },
                )
                .boxed();

            let range = logic_or
                .clone()
                .then(BinaryOp::parse_range().then(logic_or).or_not())
                .map_with(|(lhs, rhs_opt), e| match rhs_opt {
                    Some((op, rhs)) => {
                        Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                    }
                    None => lhs,
                })
                .boxed();

            choice((
                if_expr_cons(expr.clone()),
                return_expr,
                keyword("continue")
                    .map(|_| Expr::Continue)
                    .map_with(span_wrap),
                keyword("break").map(|_| Expr::Break).map_with(span_wrap),
                lambda.clone(),
                block.clone(),
                range.clone(),
            ))
            .labelled("expression")
            .padded()
            .boxed()
        });

        let let_expr = keyword("let")
            .ignore_then(var.clone())
            .then(token("=").ignore_then(expr.clone()).or_not())
            .map_with(|(var, rhs), e| {
                Expr::Let(
                    var,
                    Box::new(rhs.unwrap_or(Spanned(Expr::default(), e.span().to_end()))),
                )
            })
            .map_with(span_wrap)
            .boxed();

        let assign = var
            .clone()
            .map(|var| {
                Expr::Unary(
                    UnaryOp::Reference,
                    Box::new(var.map(|var| Expr::Location(Cell::new(Pointer::Invalid), var))),
                )
            })
            .map_with(span_wrap)
            // .or(token("*").ignore_then(expr.clone())) // TODO: fix precedence
            .then(
                token("=")
                    .labelled("binary operator")
                    .ignore_then(expr.clone()),
            )
            .map(|(lhs, rhs)| Expr::Assign(Box::new(lhs), Box::new(rhs)))
            .map_with(span_wrap)
            .boxed();

        let while_stmt = keyword("while")
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond_expr, inner_expr)| {
                Expr::While(Box::new(cond_expr), Box::new(Expr::new_ignore(inner_expr)))
            })
            .map_with(span_wrap)
            .boxed();

        let for_stmt = keyword("for")
            .ignore_then(var.clone())
            .then_ignore(keyword("in"))
            .then(expr.clone())
            .then(block.clone())
            .map(|((var, iter_expr), inner_expr)| {
                Expr::For(var, Box::new(iter_expr), Box::new(inner_expr))
            })
            .map_with(span_wrap)
            .boxed();

        let func_stmt = keyword("fn")
            .ignore_then(var.clone())
            .then(var_list.clone().delimited_by(just("("), just(")")))
            .then(block.clone())
            .map(|((name, var_names), body_expr)| {
                Expr::Function(name, Rc::new(Function(var_names, Box::new(body_expr))))
            })
            .map_with(span_wrap)
            .boxed();

        let ends_with_block = choice((
            block.clone(),
            if_expr_cons(expr.clone()),
            while_stmt.clone(),
            for_stmt.clone(),
            func_stmt.clone(),
        ))
        .boxed();

        let atom = choice((let_expr, assign, expr.clone()))
            .labelled("statement")
            .padded()
            .boxed();

        ends_with_block
            .clone()
            .then(stmt.clone())
            .map(|(first_expr, second_opt)| {
                Some(match second_opt.0 {
                    Some(_) => Expr::new_seq(first_expr, second_opt.map(Option::unwrap)),
                    None => first_expr.0,
                })
            })
            .map_with(span_wrap)
            .or(atom
                .clone()
                .or_not()
                .map_with(|opt, e| Spanned::unwrap_or_default(opt, e.span()))
                .then(
                    token(";")
                        .labelled("semicolon")
                        .ignore_then(stmt.clone())
                        .or_not(),
                )
                .map_with(|(first_expr, second_opt), e| match second_opt {
                    Some(second_opt) => Spanned(
                        Some(Expr::new_seq(
                            first_expr,
                            second_opt.map(Option::unwrap_or_default),
                        )),
                        e.span(),
                    ),
                    None => first_expr.map(Option::Some),
                }))
            .map_with(|opt: Spanned<Option<Expr>>, e| {
                opt.map(|opt| match opt {
                    Some(Expr::Let(var, val_expr)) => Some(Expr::VarScope(
                        var,
                        val_expr,
                        Box::new(Spanned(Default::default(), e.span().to_end())),
                    )),
                    Some(Expr::Function(name, func)) => Some(Expr::FunScope(
                        name,
                        func,
                        Box::new(Spanned(Default::default(), e.span().to_end())),
                    )),
                    _ => opt,
                })
            })
            .padded()
    })
    .map(|opt: Spanned<Option<Expr>>| opt.map(Option::unwrap_or_default))
    .then_ignore(end())
}
