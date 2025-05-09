use core::cell::{Cell, RefCell};

use crate::{
    expr::*,
    lexer::Token,
    span::{span_wrap, Spanned},
    typing::{BuiltinType, Polytype, Type},
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

impl BuiltinType {
    fn parse<'src, I>() -> impl Parser<'src, I, Type<'src>, ParseError<'src>> + Copy + Clone
    where
        I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
    {
        select! {
            Token::Ident(n) if n == "i32" => BuiltinType::I32,
            Token::Ident(n) if n == "bool" => BuiltinType::Bool,
            Token::Ident(n) if n == "range" => BuiltinType::Range,
        }
        .map(Type::Builtin)
    }
}

pub fn parse_stmt<'src, I>() -> impl Parser<'src, I, Spanned<Expr<'src>>, ParseError<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(
        |stmt: Recursive<dyn Parser<'_, I, Spanned<Option<Expr<'_>>>, _>>| {
            let ident = select! {
                Token::Ident(ident) => ident,
            }
            .map_with(span_wrap)
            .labelled("identifier")
            .boxed();

            let type_ = recursive(|type_| {
                let type_ref = just(Token::Op("&"))
                    .ignore_then(just(Token::Mut).or_not().map(|opt| opt.is_some()))
                    .then(type_.clone())
                    .map(|(is_mut, ty)| Type::Reference {
                        is_mut,
                        ty: Box::new(ty),
                    });

                let type_tuple = type_
                    .clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

                choice((
                    type_ref,
                    just(Token::Ident("_")).map(|_| Type::Unknown),
                    BuiltinType::parse(),
                    ident.clone().map(Type::Concrete),
                    just(Token::Fn)
                        .ignore_then(type_tuple.clone())
                        .then_ignore(just(Token::Op("->")))
                        .then(type_.clone())
                        .map(|(args, res)| Type::Function(args, Box::new(res))),
                    type_
                        .clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                    type_tuple.map(Type::Tuple),
                ))
                .labelled("type")
            });

            let var = ident
                .clone()
                .then(
                    just(Token::Ctrl(':'))
                        .labelled("type annotation")
                        .ignore_then(type_.clone())
                        .or_not()
                        .map(Option::unwrap_or_default)
                        .map(RefCell::new),
                )
                .map(|(name, type_)| Variable { name, ty: type_ })
                .boxed();

            let ret_type = just(Token::Op("->"))
                .ignore_then(type_.clone())
                .labelled("return type");

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
                        .map_with(|((cond, if_true), if_false), e| Expr::If {
                            cond: Box::new(cond),
                            if_true: Box::new(if_true),
                            if_false: Box::new(Spanned::unwrap_or_default(if_false, e.span())),
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
                    .or(just(Token::Op("||")).map(|_| Vec::new()))
                    .then(ret_type.clone().or_not().map(Option::unwrap_or_default))
                    .then(expr.clone())
                    .map(|((args, ret_type), body)| {
                        Expr::Closure(
                            Rc::new(Function::new(args, ret_type, body)),
                            RefCell::default(),
                        )
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
                    num.map(|x| Expr::Constant(Value::Number(x))),
                    bool_.map(|x| Expr::Constant(Value::Boolean(x))),
                    ident
                        .clone()
                        .map(|var| Expr::Location(Cell::new((Pointer::Invalid, 0)), var.0)),
                    expr.clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map(|s| s.0),
                    expr.clone()
                        .separated_by(just(Token::Ctrl(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map(Expr::Tuple),
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

                let unary = recursive(|unary| {
                    let unary_mut = just(Token::Op("&"))
                        .or(just(Token::Op("*")))
                        .map(Token::unwrap_op)
                        .then(just(Token::Mut).or_not().map(|opt| opt.is_some()))
                        .then(unary.clone().or(call.clone()))
                        .map(|((op, is_mut), arg)| match op {
                            "&" => Expr::Reference {
                                is_mut,
                                expr: Box::new(arg),
                            },
                            "*" => Expr::Dereference {
                                is_mut,
                                expr: Box::new(arg),
                            },
                            _ => unreachable!(),
                        })
                        .boxed();

                    UnaryOp::parse()
                        .then(unary.clone().or(call.clone()))
                        .map(|(op, arg)| Expr::Unary(op, Box::new(arg)))
                        .or(unary_mut.clone())
                        .map_with(span_wrap)
                        .or(call.clone())
                })
                .boxed();

                let cast = unary
                    .clone()
                    .foldl_with(
                        just(Token::As).then(type_.clone()).repeated(),
                        |lhs, (_, rhs), e| {
                            Spanned(
                                Expr::Cast(Box::new(lhs), Polytype::from_unknown(rhs)),
                                e.span(),
                            )
                        },
                    )
                    .boxed();

                let product = cast
                    .clone()
                    .foldl_with(
                        BinaryOp::parse_product().then(cast).repeated(),
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
                .ignore_then(just(Token::Mut).or_not().map(|opt| opt.is_some()))
                .then(var.clone())
                .then(just(Token::Op("=")).ignore_then(expr.clone()))
                .map(|((is_mut, var), val)| Expr::Let {
                    var: Variable {
                        name: var.name,
                        ty: RefCell::new(Polytype::from_unknown(var.ty.take())),
                    },
                    is_mut,
                    val: Box::new(val),
                })
                .map_with(span_wrap)
                .boxed();

            let assign = expr
                .clone()
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
                .map(|((var, iter), body)| Expr::For {
                    var,
                    iter: Box::new(iter),
                    body: Box::new(body),
                })
                .map_with(span_wrap)
                .boxed();

            let func_stmt = just(Token::Fn)
                .ignore_then(ident.clone())
                .then(
                    var_list
                        .clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .then(ret_type.clone().or_not().map(Option::unwrap_or_default))
                .then(block.clone())
                .map(|(((name, args), ret_type), body)| Expr::Function {
                    name,
                    func: Rc::new(Function::new(args, ret_type, body)),
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
            .labelled("statement")
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
                    .then(
                        just(Token::Ctrl(';'))
                            .labelled("semicolon")
                            .ignore_then(stmt.clone())
                            .or_not(),
                    )
                    .map_with(|(first_opt, second_opt), e| match (first_opt, second_opt) {
                        (Some(first_expr), Some(second_expr)) => Spanned(
                            Some(Expr::new_seq(
                                first_expr,
                                second_expr.map(Option::unwrap_or_default),
                            )),
                            e.span(),
                        ),
                        (Some(first_expr), None) => first_expr.map(Option::Some),
                        (None, Some(second_expr)) => second_expr,
                        (None, None) => Spanned(None, e.span().to_end()),
                    }))
                .map_with(|opt: Spanned<Option<Expr>>, e| {
                    opt.map(|opt| match opt {
                        Some(Expr::Let { var, is_mut, val }) => Some(Expr::VarScope {
                            var,
                            is_mut,
                            val,
                            cont: Box::new(Spanned(Default::default(), e.span().to_end())),
                        }),
                        Some(Expr::Function { name, func }) => Some(Expr::FunScope {
                            name,
                            func,
                            cont: Box::new(Spanned(Default::default(), e.span().to_end())),
                        }),
                        _ => opt,
                    })
                })
        },
    )
    .map(|opt: Spanned<Option<Expr>>| opt.map(Option::unwrap_or_default))
    .then_ignore(end())
}
