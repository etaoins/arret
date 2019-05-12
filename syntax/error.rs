use std::error;
use std::fmt;
use std::fmt::Display;
use std::result;

use crate::span::Span;

/// (Spanned)[`Span`] syntax error
#[derive(Debug, Clone, PartialEq)]
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

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.kind().message())
    }
}

/// Syntax error without (span)[`Span`] information
#[derive(Debug, Clone, PartialEq)]
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
    InvalidArgLiteral,
}

impl ErrorKind {
    /// Returns a string describing the error
    pub fn message(&self) -> String {
        match self {
            ErrorKind::Eof(ref ec) => {
                format!("unexpected end of file while parsing {}", ec.description())
            }
            ErrorKind::UnsupportedDispatch => "unsupported dispatch".to_owned(),
            ErrorKind::UnsupportedChar => "unsupported character".to_owned(),
            ErrorKind::InvalidCodePoint => "invalid code point".to_owned(),
            ErrorKind::UnsupportedStringEscape => "unsupported string escape".to_owned(),
            ErrorKind::IntegerOverflow => "integer literal does not fit in i64".to_owned(),
            ErrorKind::InvalidFloat => "unable to parse float".to_owned(),
            ErrorKind::UnexpectedChar(c) => format!("unexpected `{}`", c),
            ErrorKind::UnevenMap => "map literal must have an even number of values".to_owned(),
            ErrorKind::InvalidArgLiteral => {
                "arg literal must be `%`, `%{integer}` or `%&`".to_owned()
            }
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

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
