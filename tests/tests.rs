use chumsky::prelude::*;
use hex_coding::{expr::Expr, parser::*};

#[test]
fn test_file() {
    let src = std::fs::read_to_string("E:\\Projects\\Rust\\hex-coding\\test.txt")
        .expect("Failed to read file");

    let (ast, parse_errs) = parse_stmt().parse(src.as_str()).into_output_errors();

    if let Some(ast) = ast.filter(|_| parse_errs.is_empty()) {
        let env = Expr::set_up(&ast);
        println!("AST:\n{:#?}", ast);
        let _ = Expr::walk(&ast, |spanned_expr| {
            println!("\nElement {:?}", spanned_expr.0);
            println!(
                "Span: '{}'",
                src[spanned_expr.1.into_range()]
                    .to_string()
                    .trim()
                    .replace("\r\n", "\\n")
            );
            true
        });
        println!("\nReturn value: {:?}", ast.eval(env));
    } else {
        parse_errs
            .iter()
            .for_each(|e| println!("parsing error: {}", e.to_string()));
    }
    assert!(parse_errs.is_empty());
}
