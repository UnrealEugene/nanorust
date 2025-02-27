use core::fmt::{self, Display};

use alloc::vec::Vec;
use chumsky::prelude::*;

use crate::span::{span_wrap, Spanned};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'str> {
    Bool(bool),
    Num(i32),
    Op(&'str str),
    Ctrl(char),
    Ident(&'str str),
    Fn,
    Let,
    If,
    Else,
    While,
    For,
    In,
    Return,
    Break,
    Continue,
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{}", x),
            Token::Num(x) => write!(f, "{}", x),
            Token::Op(x) => write!(f, "{}", x),
            Token::Ctrl(x) => write!(f, "{}", x),
            Token::Ident(x) => write!(f, "{}", x),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Return => write!(f, "return"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char>>> {
    let bool_ = choice((
        just("true").to(Token::Bool(true)),
        just("false").to(Token::Bool(false)),
    ))
    .labelled("literal")
    .boxed();

    let num = text::int(10)
        .from_str()
        .unwrapped()
        .map(Token::Num)
        .labelled("literal")
        .boxed();

    let op = choice((
        just("..="),
        just(".."),
        just("=="),
        just("!="),
        just("<="),
        just(">="),
        just("&&"),
        just("||"),
        just("=="),
        just("="),
        just("<"),
        just(">"),
        just("+"),
        just("-"),
        just("*"),
        just("/"),
        just("%"),
        just("!"),
        just("&"),
    ))
    .map(|s| Token::Op(s))
    .labelled("operator")
    .boxed();

    let ctrl = one_of("(){}[]|;,").map(Token::Ctrl).boxed();

    let ident = text::ident()
        .map(|ident: &'src str| match ident {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "return" => Token::Return,
            "break" => Token::Break,
            "continue" => Token::Continue,
            _ => Token::Ident(ident),
        })
        .boxed();

    let token = bool_.or(num).or(op).or(ctrl).or(ident).labelled("token");

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .labelled("comment")
        .padded();

    token
        .map_with(span_wrap)
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
