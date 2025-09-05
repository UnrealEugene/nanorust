#![forbid(unsafe_code)]
#![no_std]

extern crate slog;
extern crate slog_term;
extern crate alloc;
extern crate core;
extern crate nanorust_macros;

extern crate self as nanorust;

pub mod span;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod expr;
pub mod eval;
pub mod typing;