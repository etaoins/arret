use syntax::span::Span;
use syntax::error::Error as SyntaxError;

#[derive(Debug, PartialEq)]
pub enum Error {
    PrimitiveRef(Span, String),
    UnboundSymbol(Span, String),
    WrongArgCount(Span, usize),
    IllegalArg(Span, String),
    ExpectedSymbol(Span),
    DefOutsideBody(Span),
    ExportOutsideModule(Span),
    LibraryNotFound(Span),
    ReadError(String),
    SyntaxError(SyntaxError),
}
