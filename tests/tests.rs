use chumsky::prelude::*;
use hex_coding::parser::*;

#[test]
fn test_file() {
    let src = std::fs::read_to_string("E:\\Projects\\Rust\\hex-coding\\test.txt").unwrap();

    match stmt().parse(src) {
        Ok(mut ast) => {
            ast.set_up();
            println!("{:#?}", ast);
            println!("{:?}", ast.eval())
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
