use chumsky::{
    extra::ParserExtra,
    input::{Input, MapExtra},
    span::{SimpleSpan, Span},
};

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub SimpleSpan);

impl<T: Copy> Copy for Spanned<T> {}

impl<T> Spanned<T> {
    pub fn span(&self) -> SimpleSpan {
        self.1
    }

    pub fn map<R, F: FnMut(T) -> R>(self, mut f: F) -> Spanned<R> {
        Spanned(f(self.0), self.1)
    }
}

impl<T: Default> Spanned<T> {}

impl<T: Default> Spanned<T> {
    pub fn unwrap_or_default(opt: Option<Spanned<T>>, span: SimpleSpan) -> Spanned<T> {
        opt.unwrap_or(Spanned(Default::default(), span.to_end()))
    }
}

pub fn span_wrap<'src, T, I, E>(x: T, e: &mut MapExtra<'src, '_, I, E>) -> Spanned<T>
where
    I: Input<'src, Span = SimpleSpan>,
    E: ParserExtra<'src, I>,
{
    Spanned(x, e.span())
}
