#![forbid(unsafe_code)]
#![no_std]

extern crate alloc;
extern crate core;
extern crate hex_coding_macros;

extern crate self as hex_coding;

pub mod span;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod expr;
pub mod eval;
pub mod typing;