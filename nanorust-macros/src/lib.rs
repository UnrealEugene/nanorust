extern crate proc_macro;
use chumsky::prelude::*;
use proc_macro::TokenStream;

const QUALIFIED_BOX: &str = "::alloc::boxed::Box";
const QUALIFIED_TYPE_VAR: &str = "::nanorust::typing::TypeVar";
const QUALIFIED_BUILTIN_TYPE: &str = "::nanorust::typing::BuiltinType";
const QUALIFIED_TYPE: &str = "::nanorust::typing::Type";
const QUALIFIED_POLYTYPE: &str = "::nanorust::typing::Polytype::<'static>";

fn vec_to_code(vec: Vec<String>) -> String {
    let mut inside_str = String::new();
    let mut first = true;
    for elem_str in vec.into_iter() {
        if !first {
            inside_str += ", ";
        }
        inside_str += elem_str.as_str();
        first = false;
    }
    format!("::alloc::vec![{inside_str}]")
}

fn type_parser<'src>() -> impl Parser<'src, &'src str, String> {
    let token = |s| just(s).padded();

    let var = just("'")
        .ignore_then(text::int(10))
        .map(|s| format!("{QUALIFIED_TYPE}::Variable({QUALIFIED_TYPE_VAR}({s}))"));

    let builtin = choice((
        just("i32").to("I32"),
        just("bool").to("Bool"),
        just("range").to("Range"),
    ))
    .map(|s| format!("{QUALIFIED_TYPE}::Builtin({QUALIFIED_BUILTIN_TYPE}::{s})"));

    recursive(|type_| {
        let type_ref = token("&")
            .ignore_then(token("mut").or_not().map(|opt| opt.is_some()))
            .then(type_.clone())
            .map(|(is_mut, ty)| format!("{QUALIFIED_TYPE}::Reference{{ is_mut: {is_mut}, ty: {QUALIFIED_BOX}::new({ty})}}"));

        let type_tuple = type_
            .clone()
            .separated_by(just(","))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(token("("), token(")"));

        choice((
            type_ref,
            token("fn")
                .ignore_then(type_tuple.clone())
                .then_ignore(token("->"))
                .then(type_.clone())
                .map(|(args, res)| {
                    format!(
                        "{QUALIFIED_TYPE}::Function({}, {QUALIFIED_BOX}::new({res}))",
                        vec_to_code(args)
                    )
                }),
            var,
            builtin,
            text::ident().map(|s| format!("{QUALIFIED_TYPE}::Concrete(\"{s}\")")),
            type_.clone().delimited_by(token("("), token(")")),
            type_tuple.map(|args| format!("{QUALIFIED_TYPE}::Tuple({})", vec_to_code(args))),
        ))
        .padded()
    })
    .map(|s| format!("{QUALIFIED_POLYTYPE}::new_generalized({s})"))
}

#[proc_macro]
pub fn ty(stream: TokenStream) -> TokenStream {
    let input = stream.to_string();
    let type_str = type_parser()
        .parse(input.trim_matches('"'))
        .into_output()
        .expect("input type string parse error");
    type_str.parse().expect("output source code parse error")
}
