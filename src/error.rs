use core::ops::Range;

use alloc::{format, string::String};
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use chumsky::span::SimpleSpan;
use yansi::Color;

use crate::{
    parser::{Identifier, Keyword},
    typing::Type,
};

fn plural(n: usize, word: &str) -> String {
    match n {
        1 => format!("{} {}", n, word),
        _ => format!("{} {}s", n, word),
    }
}

pub type SemanticReport = ReportBuilder<'static, (&'static str, Range<usize>)>;

pub trait SemanticError {
    fn report(&self, file_name: &'static str) -> SemanticReport;
    fn code(&self) -> usize;
}

pub struct UndefinedSymbolError<'src> {
    pub ident: Identifier<'src>,
}

impl SemanticError for UndefinedSymbolError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.ident.1.into_range()))
            .with_message(format!(
                "cannot find symbol `{}` in this scope",
                self.ident.0
            ))
            .with_label(
                Label::new((file_name, self.ident.1.into_range()))
                    .with_message("not found in this scope")
                    .with_color(Color::Red),
            )
    }

    fn code(&self) -> usize {
        1
    }
}

pub struct BreakOutsideOfLoopError<'src> {
    pub keyword: Keyword<'src>,
}

impl SemanticError for BreakOutsideOfLoopError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.keyword.1.into_range()))
            .with_message(format!("`{}` outside of loop", self.keyword.0))
            .with_label(
                Label::new((file_name, self.keyword.1.into_range()))
                    .with_message(format!("cannot `{}` outside of loop", self.keyword.0))
                    .with_color(Color::Red),
            )
    }

    fn code(&self) -> usize {
        2
    }
}

pub struct FunctionCaptureError<'src> {
    pub ident: Identifier<'src>,
    pub other_ident: Identifier<'src>,
}

impl SemanticError for FunctionCaptureError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.ident.1.into_range()))
            .with_message(format!(
                "cannot capture variable `{}` in a fn item",
                self.ident.0
            ))
            .with_label(
                Label::new((file_name, self.ident.1.into_range()))
                    .with_message("cannot capture a variable in a fn item")
                    .with_color(Color::Red),
            )
            .with_label(
                Label::new((file_name, self.other_ident.1.into_range()))
                    .with_message("variable is defined outside of fn item")
                    .with_color(Color::Cyan),
            )
            .with_help("use the `|| { ... }` closure form instead")
    }

    fn code(&self) -> usize {
        3
    }
}

pub struct FunctionRedefinitionError<'src> {
    pub name: &'src str,
    pub func_span: SimpleSpan,
    pub other_func_span: SimpleSpan,
}

impl SemanticError for FunctionRedefinitionError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.func_span.into_range()))
            .with_message(format!(
                "function `{}` is defined multiple times",
                self.name
            ))
            .with_label(
                Label::new((file_name, self.func_span.into_range()))
                    .with_message(format!("`{}` redefined here", self.name))
                    .with_color(Color::Red),
            )
            .with_label(
                Label::new((file_name, self.other_func_span.into_range()))
                    .with_message(format!(
                        "previous definition of function `{}` is here",
                        self.name
                    ))
                    .with_color(Color::Cyan),
            )
            .with_note(format!(
                "`{}` must be defined only once in this block",
                self.name
            ))
    }

    fn code(&self) -> usize {
        4
    }
}

pub struct TypeMismatchError<'src> {
    pub span: SimpleSpan,
    pub expected_type: Type<'src>,
    pub actual_type: Type<'src>,
}

impl SemanticError for TypeMismatchError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.span.into_range()))
            .with_message("mismatched types")
            .with_label(
                Label::new((file_name, self.span.into_range()))
                    .with_message(format!(
                        "expected `{}`, found `{}`",
                        self.expected_type, self.actual_type
                    ))
                    .with_color(Color::Red),
            )
    }

    fn code(&self) -> usize {
        5
    }
}

pub struct RecursiveTypeError<'src> {
    pub span: SimpleSpan,
    pub first_type: Type<'src>,
    pub second_type: Type<'src>,
}

impl SemanticError for RecursiveTypeError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.span.into_range()))
            .with_message("type unification occurs check failed")
            .with_label(
                Label::new((file_name, self.span.into_range()))
                    .with_message(format!(
                        "cannot have a value which is both `{}` and `{}`",
                        self.first_type, self.second_type
                    ))
                    .with_color(Color::Red),
            )
    }

    fn code(&self) -> usize {
        6
    }
}

pub struct IllegalAssignmentError {
    pub span: SimpleSpan,
}

impl SemanticError for IllegalAssignmentError {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(ReportKind::Error, (file_name, self.span.into_range()))
            .with_message("invalid left-hand side of assignment")
            .with_label(
                Label::new((file_name, self.span.into_range()))
                    .with_message("cannot assign to this expression")
                    .with_color(Color::Red),
            )
    }

    fn code(&self) -> usize {
        7
    }
}

pub struct ImmutableAssignmentError<'src> {
    pub assign_span: SimpleSpan,
    pub var_def: Identifier<'src>,
}

impl SemanticError for ImmutableAssignmentError<'_> {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        Report::build(
            ReportKind::Error,
            (file_name, self.assign_span.into_range()),
        )
        .with_message(format!(
            "cannot assign twice to immutable variable `{}`",
            self.var_def.0
        ))
        .with_label(
            Label::new((file_name, self.assign_span.into_range()))
                .with_message("cannot assign twice to immutable variable")
                .with_color(Color::Red),
        )
        .with_label(
            Label::new((file_name, self.var_def.1.into_range()))
                .with_message(format!("first assignment to `{}`", self.var_def.0))
                .with_color(Color::Cyan),
        )
    }

    fn code(&self) -> usize {
        8
    }
}

pub struct ArgumentCountMismatchError {
    pub callee_span: SimpleSpan,
    pub expected_count: usize,
    pub actual_count: usize,
}

impl SemanticError for ArgumentCountMismatchError {
    fn report(&self, file_name: &'static str) -> SemanticReport {
        let message = format!(
            "this callable takes {} but {} were supplied",
            plural(self.expected_count, "argument"),
            plural(self.actual_count, "argument"),
        );
        Report::build(
            ReportKind::Error,
            (file_name, self.callee_span.into_range()),
        )
        .with_message(&message)
        .with_label(
            Label::new((file_name, self.callee_span.into_range()))
                .with_message(&message)
                .with_color(Color::Red),
        )
    }

    fn code(&self) -> usize {
        9
    }
}
