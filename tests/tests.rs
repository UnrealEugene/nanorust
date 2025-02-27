use std::path::Path;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use hex_coding::{expr::Expr, lexer::lexer, parser::*};

#[test]
fn test_file() {
    let file_path = Path::new("E:\\Projects\\Rust\\hex-coding\\test.txt");
    
    let file_name = file_path.file_name().unwrap().to_str().unwrap().to_string();

    let src = std::fs::read_to_string(file_path).expect("Failed to read file");

    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parse_stmt()
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |x| (&x.0, &x.1)),
            )
            .into_output_errors();

        if let Some(ast) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            let env = Expr::set_up(&ast);
            println!("Return value: {:?}", ast.eval(env));
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, (file_name.clone(), e.span().into_range()))
                .with_message(e.to_string())
                .with_label(
                    Label::new((file_name.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((file_name.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(file_name.clone(), src.clone())]))
                .unwrap()
        });
}
