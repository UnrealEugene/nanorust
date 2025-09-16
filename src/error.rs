use core::ops::Range;

use alloc::format;
use ariadne::{Label, Report, ReportBuilder, ReportKind};
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
