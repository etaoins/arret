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
    Eof(WithinContext),
    UnsupportedDispatch,
    UnsupportedChar,
    InvalidCodePoint,
    UnsupportedStringEscape,
    IntegerOverflow,
    InvalidFloat,
    UnexpectedChar(char, WithinContext),
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
            ErrorKind::UnexpectedChar(c, ec) => {
                format!("unexpected `{}` while parsing {}", c, ec.description())
            }
            ErrorKind::UnevenMap => "map literal must have an even number of values".to_owned(),
            ErrorKind::InvalidArgLiteral => {
                "arg literal must be `%`, `%{integer}` or `%&`".to_owned()
            }
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

/// Describes the content an error occurred within, with optional starting span
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum WithinContext {
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

impl WithinContext {
    /// Returns a description of the content that was being parsed
    pub fn description(&self) -> &'static str {
        match self {
            WithinContext::List(_) => "list",
            WithinContext::Vector(_) => "vector",
            WithinContext::Set(_) => "set",
            WithinContext::Map(_) => "map",
            WithinContext::String(_) => "string literal",
            WithinContext::Identifier => "identifier",
            WithinContext::Datum => "datum",
            WithinContext::Dispatch => "dispatch",
            WithinContext::QuoteEscape => "quote escape",
            WithinContext::CodePoint => "code point",
        }
    }

    /// Returns the normally expected in this context
    pub fn expected_next(&self) -> Option<ExpectedNext> {
        match self {
            WithinContext::List(_) => Some(ExpectedNext::List),
            WithinContext::Vector(_) => Some(ExpectedNext::Vector),
            WithinContext::Set(_) => Some(ExpectedNext::Set),
            WithinContext::Map(_) => Some(ExpectedNext::Map),
            WithinContext::String(_) => Some(ExpectedNext::String),
            _ => None,
        }
    }

    /// Returns the character opening the sequence or string
    pub fn open_char_span(&self) -> Option<Span> {
        match self {
            WithinContext::List(span)
            | WithinContext::Vector(span)
            | WithinContext::Set(span)
            | WithinContext::Map(span)
            | WithinContext::String(span) => Some(*span),
            _ => None,
        }
    }
}

/// Describes the content normally expected within the content
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExpectedNext {
    List,
    Vector,
    Set,
    Map,
    String,
}

impl ExpectedNext {
    /// Returns the character that would terminate this sequence or string
    pub fn close_char(self) -> char {
        match self {
            ExpectedNext::List => ')',
            ExpectedNext::Vector => ']',
            ExpectedNext::Set => '}',
            ExpectedNext::Map => '}',
            ExpectedNext::String => '"',
        }
    }

    pub fn description(self) -> String {
        match self {
            ExpectedNext::String => "expected `\"`".to_owned(),
            other => format!("expected datum or `{}`", other.close_char()),
        }
    }
}
