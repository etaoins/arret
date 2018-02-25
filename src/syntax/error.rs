use std::result;
use std::fmt;
use std::fmt::Display;
use std::error;

use syntax::span::Span;
use reporting::Reportable;

#[derive(Debug, PartialEq)]
pub enum Error {
    Eof(Span, &'static str),
    TrailingCharacters(Span),
    InvalidDispatch(Span),
    InvalidCharLiteral(Span),
    InvalidCharCodePoint(Span),
    InvalidQuoteEscape(Span),
    InvalidInteger(Span),
    IntegerOverflow(Span),
    UnexpectedChar(Span, char),
    UnevenMap(Span),
}

pub type Result<T> = result::Result<T, Error>;

impl Reportable for Error {
    fn message(&self) -> String {
        match *self {
            Error::Eof(_, context_desc) => {
                format!("Unexpected end of file while parsing {}", context_desc)
            }
            _ => "Syntax error".to_owned(),
        }
    }

    fn span(&self) -> Option<Span> {
        match *self {
            Error::Eof(span, _) => Some(span),
            _ => None,
        }
    }
}

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
