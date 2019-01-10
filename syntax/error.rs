use std::error;
use std::fmt;
use std::fmt::Display;
use std::result;

use crate::span::Span;

#[derive(Debug, PartialEq)]
pub struct Error {
    span: Span,
    pub(crate) kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error { span, kind }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Eof(ExpectedContent),
    UnsupportedDispatch,
    UnsupportedChar,
    InvalidCodePoint,
    UnsupportedStringEscape,
    IntegerOverflow,
    InvalidFloat,
    UnexpectedChar(char),
    UnevenMap,
}

pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {
    fn description(&self) -> &str {
        "Reader error"
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExpectedContent {
    List(Span),
    Vector(Span),
    Set(Span),
    Map(Span),
    String(Span),
    Identifier,
    Datum,
    Dispatch,
    QuoteEscape,
    CodePoint,
}

impl ExpectedContent {
    pub fn description(&self) -> &'static str {
        match self {
            ExpectedContent::List(_) => "list",
            ExpectedContent::Vector(_) => "vector",
            ExpectedContent::Set(_) => "set",
            ExpectedContent::Map(_) => "map",
            ExpectedContent::String(_) => "string literal",
            ExpectedContent::Identifier => "identifier",
            ExpectedContent::Datum => "datum",
            ExpectedContent::Dispatch => "dispatch",
            ExpectedContent::QuoteEscape => "quote escape",
            ExpectedContent::CodePoint => "code point",
        }
    }

    pub fn open_char_span(&self) -> Option<Span> {
        match self {
            ExpectedContent::List(span)
            | ExpectedContent::Vector(span)
            | ExpectedContent::Set(span)
            | ExpectedContent::Map(span)
            | ExpectedContent::String(span) => Some(*span),
            _ => None,
        }
    }
}
