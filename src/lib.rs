#![forbid(unsafe_code)]
// #![no_std]

extern crate alloc;
extern crate core;
extern crate nanorust_macros;
extern crate slog;
extern crate slog_term;

extern crate self as nanorust;

pub mod error;
pub mod expr;
pub mod interpret;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod typing;
pub mod value;
