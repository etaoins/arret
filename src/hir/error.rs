use syntax::span::Span;

#[derive(Debug, PartialEq)]
pub enum Error {
    PrimitiveRef(Span, String),
    UnboundSymbol(Span, String),
    WrongArgCount(Span, usize),
    ExpectedDefSymbol(Span),
}
