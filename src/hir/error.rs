use syntax::span::Span;

#[derive(Debug, PartialEq)]
pub enum Error {
    PrimitiveRef(Span, String),
    UnboundSymbol(Span, String),
    WrongArgCount(Span, usize),
    IllegalArg(Span, String),
    ExpectedSymbol(Span),
    DefOutsideBody(Span),
    ExportOutsideModule(Span),
}
