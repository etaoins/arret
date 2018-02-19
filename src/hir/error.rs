use std::{error, result};
use std::fmt;
use std::fmt::Display;

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

pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {
    fn description(&self) -> &str {
        "Lowering error"
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<SyntaxError> for Error {
    fn from(err: SyntaxError) -> Error {
        Error::SyntaxError(err)
    }
}
