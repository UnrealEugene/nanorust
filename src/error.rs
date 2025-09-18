use core::ops::Range;

use alloc::format;
use ariadne::{Label, Report, ReportBuilder, ReportKind};
use chumsky::span::SimpleSpan;
use yansi::Color;

use crate::parser::{Identifier, Keyword};

pub type SemanticReport = ReportBuilder<'static, (&'static str, Range<usize>)>;

pub trait SemanticError {
    fn report(&self, file_name: &'static str) -> SemanticReport;
    fn code(&self) -> usize;
}

pub struct UndefinedSymbolError<'src> {
    pub ident: Identifier<'src>,
}

impl<'src> SemanticError for UndefinedSymbolError<'src> {
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

impl<'src> SemanticError for BreakOutsideOfLoopError<'src> {
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

impl<'src> SemanticError for FunctionCaptureError<'src> {
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

impl<'src> SemanticError for FunctionRedefinitionError<'src> {
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
