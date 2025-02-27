use core::cell::Cell;

use crate::{
    expr::*,
    lexer::Token,
    span::{span_wrap, Spanned},
};
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use chumsky::{input::ValueInput, prelude::*};

type ParseError<'src> = extra::Err<Rich<'src, Token<'src>>>;

impl UnaryOp {
    fn parse<'src, I>() -> impl Parser<'src, I, UnaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "-" => UnaryOp::Negate,
            Token::Op(op) if op == "!" => UnaryOp::LogicNot,
            Token::Op(op) if op == "&" => UnaryOp::Reference,
            Token::Op(op) if op == "*" => UnaryOp::Dereference,
        }
        .labelled("unary operator")
    }
}

impl BinaryOp {
    fn parse_sum<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "+" => BinaryOp::Add,
            Token::Op(op) if op == "-" => BinaryOp::Subtract,
        }
        .labelled("binary operator")
    }

    fn parse_product<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "*" => BinaryOp::Multiply,
            Token::Op(op) if op == "/" => BinaryOp::Divide,
            Token::Op(op) if op == "%" => BinaryOp::Modulo,
        }
        .labelled("binary operator")
    }

    fn parse_compare<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "==" => BinaryOp::Equal,
            Token::Op(op) if op == "!=" => BinaryOp::NotEqual,
            Token::Op(op) if op == "<=" => BinaryOp::LessEqual,
            Token::Op(op) if op == ">=" => BinaryOp::GreaterEqual,
            Token::Op(op) if op == "<" => BinaryOp::Less,
            Token::Op(op) if op == ">" => BinaryOp::Greater,
        }
        .labelled("binary operator")
    }

    fn parse_logic_and<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "&&" => BinaryOp::LogicAnd,
        }
        .labelled("binary operator")
    }

    fn parse_logic_or<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "||" => BinaryOp::LogicOr,
        }
        .labelled("binary operator")
    }

    fn parse_range<'src, I>() -> impl Parser<'src, I, BinaryOp, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Op(op) if op == "..=" => BinaryOp::RangeInclusive,
            Token::Op(op) if op == ".." => BinaryOp::Range,
        }
        .labelled("binary operator")
    }
}

pub fn parse_stmt<'src, I>() -> impl Parser<'src, I, Spanned<Expr<'src>>, ParseError<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(
        |stmt: Recursive<dyn Parser<'_, I, Spanned<Option<Expr<'_>>>, _>>| {
            // let var = {
            //     chumsky::primitive::select(move |x, extra| match x {
            //         Token::Ident(var) => Some(var),
            //         _ => None,
            //     })
            //     .map_with(span_wrap)
            // };

            let var = select! {
                Token::Ident(var) => var,
            }
            .map_with(span_wrap)
            .labelled("identifier");

            let bool_ = select! { Token::Bool(x) => x };

            let num = select! {
                Token::Num(x) => x,
            }
            .labelled("integer");

            let block = stmt
                .clone()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                .map(|opt: Spanned<Option<Expr>>| {
                    Expr::Block(Box::new(opt.map(Option::unwrap_or_default)))
                })
                .map_with(span_wrap)
                .labelled("block")
                .boxed();

            let if_expr_cons = |expr| {
                recursive(|if_stmt| {
                    just(Token::If)
                        .ignore_then(expr)
                        .then(block.clone())
                        .then(
                            just(Token::Else)
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
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .boxed();

            let expr = recursive(|expr| {
                let lambda = var_list
                    .clone()
                    .delimited_by(just(Token::Ctrl('|')), just(Token::Ctrl('|')))
                    .then(expr.clone())
                    .map(|(var_names, body_expr)| {
                        Expr::Closure(Rc::new(Function(var_names, Box::new(body_expr))))
                    })
                    .map_with(span_wrap)
                    .boxed();

                let return_expr = just(Token::Return)
                    .ignore_then(expr.clone().or_not())
                    .map_with(|opt, e| {
                        Expr::Return(Box::new(Spanned::unwrap_or_default(opt, e.span())))
                    })
                    .map_with(span_wrap)
                    .boxed();

                let atom = choice((
                    expr.clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map(|s| s.0),
                    bool_.map(|x| Expr::Constant(Value::Boolean(x))),
                    var.clone()
                        .map(|var| Expr::Location(Cell::new(Pointer::Invalid), var.0)),
                    num.map(|x| Expr::Constant(Value::Number(x))),
                ))
                .map_with(span_wrap)
                .labelled("expression")
                .boxed();

                let expr_list = expr
                    .clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .boxed();

                let call = atom
                    .clone()
                    .foldl_with(
                        expr_list
                            .clone()
                            .delimited_by(
                                just(Token::Ctrl('(')).labelled("function call"),
                                just(Token::Ctrl(')')),
                            )
                            .repeated(),
                        |fn_expr, arg_list, e| {
                            Spanned(Expr::Call(Box::new(fn_expr), arg_list), e.span())
                        },
                    )
                    .boxed();

                let unary = UnaryOp::parse()
                    .labelled("expression")
                    .repeated()
                    .foldr_with(call.clone(), |op, x, e| {
                        Spanned(Expr::Unary(op, Box::new(x)), e.span())
                    })
                    .boxed();

                let product = unary
                    .clone()
                    .foldl_with(
                        BinaryOp::parse_product().then(unary).repeated(),
                        |lhs, (op, rhs), e| {
                            Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                        },
                    )
                    .boxed();

                let sum = product
                    .clone()
                    .foldl_with(
                        BinaryOp::parse_sum().then(product).repeated(),
                        |lhs, (op, rhs), e| {
                            Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                        },
                    )
                    .boxed();

                let compare = sum
                    .clone()
                    .then(BinaryOp::parse_compare().then(sum).or_not())
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
                        BinaryOp::parse_logic_and().then(compare).repeated(),
                        |lhs, (op, rhs), e| {
                            Spanned(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), e.span())
                        },
                    )
                    .boxed();

                let logic_or = logic_and
                    .clone()
                    .foldl_with(
                        BinaryOp::parse_logic_or().then(logic_and).repeated(),
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
                    just(Token::Continue)
                        .map(|_| Expr::Continue)
                        .map_with(span_wrap),
                    just(Token::Break).map(|_| Expr::Break).map_with(span_wrap),
                    lambda.clone(),
                    block.clone(),
                    range.clone(),
                ))
                .labelled("expression")
                .boxed()
            });

            let let_expr = just(Token::Let)
                .ignore_then(var.clone())
                .then(just(Token::Op("=")).ignore_then(expr.clone()).or_not())
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
                    just(Token::Op("="))
                        .labelled("binary operator")
                        .ignore_then(expr.clone()),
                )
                .map(|(lhs, rhs)| Expr::Assign(Box::new(lhs), Box::new(rhs)))
                .map_with(span_wrap)
                .boxed();

            let while_stmt = just(Token::While)
                .ignore_then(expr.clone())
                .then(block.clone())
                .map(|(cond_expr, inner_expr)| {
                    Expr::While(Box::new(cond_expr), Box::new(Expr::new_ignore(inner_expr)))
                })
                .map_with(span_wrap)
                .boxed();

            let for_stmt = just(Token::For)
                .ignore_then(var.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .then(block.clone())
                .map(|((var, iter_expr), inner_expr)| {
                    Expr::For(var, Box::new(iter_expr), Box::new(inner_expr))
                })
                .map_with(span_wrap)
                .boxed();

            let func_stmt = just(Token::Fn)
                .ignore_then(var.clone())
                .then(
                    var_list
                        .clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
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
                        just(Token::Ctrl(';'))
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
        },
    )
    .map(|opt: Spanned<Option<Expr>>| opt.map(Option::unwrap_or_default))
    .then_ignore(end())
}
