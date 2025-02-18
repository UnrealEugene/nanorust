use chumsky::prelude::*;
use hex_coding::parser::*;

#[test]
fn test_file() {
    let src = std::fs::read_to_string("E:\\Projects\\Rust\\hex-coding\\test.txt")
        .expect("Failed to read file");

    let (ast, parse_errs) = parse_stmt().parse(src.as_str()).into_output_errors();

    if let Some(ast) = ast.filter(|_| parse_errs.is_empty()) {
        let env = ast.set_up();
        println!("AST:\n{:#?}", ast);
        println!("Return value: {:?}", ast.eval(env));
    } else {
        parse_errs.iter().for_each(|e| println!("parsing error: {}", e.to_string()));
    }
    assert!(parse_errs.is_empty());
}
