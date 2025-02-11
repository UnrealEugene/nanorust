use crate::expr::*;
use alloc::{boxed::Box, string::String, vec};
use chumsky::prelude::*;

impl UnaryOp {
    fn parse() -> impl Parser<char, UnaryOp, Error = Simple<char>> + Clone {
        choice((
            just("-").to(UnaryOp::Negate),
            just("!").to(UnaryOp::LogicNot),
            just("&").to(UnaryOp::Reference),
            just("*").to(UnaryOp::Dereference),
        ))
    }
}

impl BinaryOp {
    fn parse_sum() -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        choice((
            just("+").to(BinaryOp::Add),
            just("-").to(BinaryOp::Subtract),
        ))
    }

    fn parse_product() -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        choice((
            just("*").to(BinaryOp::Multiply),
            just("/").to(BinaryOp::Divide),
        ))
    }

    fn parse_compare() -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        choice((
            just("==").to(BinaryOp::Equal),
            just("!=").to(BinaryOp::NotEqual),
            just("<=").to(BinaryOp::LessEqual),
            just(">=").to(BinaryOp::GreaterEqual),
            just("<").to(BinaryOp::Less),
            just(">").to(BinaryOp::Greater),
        ))
    }

    fn parse_logic_and() -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        just("&&").to(BinaryOp::LogicAnd)
    }

    fn parse_logic_or() -> impl Parser<char, BinaryOp, Error = Simple<char>> + Clone {
        just("||").to(BinaryOp::LogicOr)
    }
}

pub fn stmt() -> impl Parser<char, Expr, Error = Simple<char>> {
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

        let var_list = var
            .clone()
            .map(|e| vec![e])
            .then(token(",").then(var.clone()).repeated())
            .foldl(|mut a, b| {
                a.push(b.1);
                a
            })
            .then(token(",").or_not())
            .or_not()
            .map(|opt| opt.map(|x| x.0).unwrap_or_default())
            .padded();

        let expr = recursive(|expr| {
            let lambda = token("|")
                .then(var_list.clone())
                .then(token("|"))
                .then(expr.clone())
                .map(|(((_, var_names), _), body_expr)| {
                    Expr::Function(var_names, Box::new(body_expr))
                })
                .padded();

            let return_expr = token("return")
                .then(expr.clone())
                .map(|(_, val)| Expr::Return(Box::new(val)));

            let atom = choice((
                expr.clone().delimited_by(just('('), just(')')),
                var.clone().map(|var| Expr::Location(Pointer::Invalid, var)),
                num,
            ))
            .padded();

            let expr_list = expr
                .clone()
                .map(|e| vec![e])
                .then(token(",").then(expr.clone()).repeated())
                .foldl(|mut a, b| {
                    a.push(b.1);
                    a
                })
                .then(token(",").or_not())
                .or_not()
                .map(|opt| opt.map(|x| x.0).unwrap_or_default())
                .padded();

            let call = atom
                .clone()
                .then(
                    expr_list
                        .clone()
                        .delimited_by(just("("), just(")"))
                        .repeated(),
                )
                .foldl(|fn_expr, arg_list| Expr::Call(Box::new(fn_expr), arg_list))
                .padded();

            let unary = UnaryOp::parse()
                .padded()
                .repeated()
                .then(call.clone())
                .foldr(|op, x| Expr::Unary(op, Box::new(x)));

            let product = unary
                .clone()
                .then(BinaryOp::parse_product().padded().then(unary).repeated())
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            let sum = product
                .clone()
                .then(BinaryOp::parse_sum().then(product).repeated())
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            let compare = sum
                .clone()
                .then(BinaryOp::parse_compare().then(sum).or_not())
                .map(|(lhs, rhs_opt)| match rhs_opt {
                    Some((op, rhs)) => Expr::Binary(op, Box::new(lhs), Box::new(rhs)),
                    None => lhs,
                });

            let logic_and = compare
                .clone()
                .then(BinaryOp::parse_logic_and().then(compare).repeated())
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            let logic_or = logic_and
                .clone()
                .then(BinaryOp::parse_logic_or().then(logic_and).repeated())
                .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

            choice((
                if_expr_cons(expr.clone()),
                return_expr,
                lambda.clone(),
                block.clone(),
                logic_or.clone(),
            ))
            .padded()
        });

        let let_expr = token("let")
            .then(var)
            .then(token("=").then(expr.clone()).or_not())
            .map(|((_, var), rhs)| {
                Expr::Let(var, Box::new(rhs.map(|x| x.1).unwrap_or(Expr::Constant(0))))
            });

        let assign = var
            .clone()
            .map(|var| {
                Expr::Unary(
                    UnaryOp::Reference,
                    Box::new(Expr::Location(Pointer::Invalid, var)),
                )
            })
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
            .map(|expr_opt| match expr_opt {
                Some(Expr::Let(var, val_expr)) => {
                    Some(Expr::Scope(var, val_expr, Box::new(Expr::Skip)))
                }
                _ => expr_opt,
            })
            .padded()
    })
    .map(|opt: Option<Expr>| opt.unwrap_or(Expr::Skip))
    .then_ignore(end())
}
