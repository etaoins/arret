use std::result;
use std::fmt;
use std::fmt::Display;
use std::error;
use syntax::span::Span;

#[derive(Debug, PartialEq)]
pub enum Error {
    Eof,
    TrailingCharacters(Span),
    InvalidOctoDatum(Span),
    InvalidCharLiteral(Span),
    InvalidCharCodePoint(Span),
    InvalidQuoteEscape(Span),
    InvalidInteger(Span),
    IntegerOverflow(Span),
    ExpectedLiteral(Span, &'static str),
    UnexpectedChar(Span, char),
    UnevenMap(Span),
}

pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {
    fn description(&self) -> &str {
        "Reader error"
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
