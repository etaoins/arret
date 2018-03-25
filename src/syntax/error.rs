use std::result;
use std::fmt;
use std::fmt::Display;
use std::error;

use syntax::span::Span;
use reporting::{Level, Reportable};

#[derive(Debug, PartialEq)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error { span, kind }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
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
    UnexpectedChar(char),
    UnevenMap,
}

pub type Result<T> = result::Result<T, Error>;

impl Reportable for Error {
    fn message(&self) -> String {
        match self.kind {
            ErrorKind::Eof(ref ec) => {
                format!("unexpected end of file while parsing {}", ec.description())
            }
            ErrorKind::UnsupportedDispatch => "unsupported dispatch".to_owned(),
            ErrorKind::UnsupportedChar => "unsupported character".to_owned(),
            ErrorKind::InvalidCodePoint => "invalid code point".to_owned(),
            ErrorKind::UnsupportedStringEscape => "unsupported string escape".to_owned(),
            ErrorKind::IntegerOverflow => "integer literal does not fit in i64".to_owned(),
            ErrorKind::UnexpectedChar(c) => format!("unexpected `{}`", c),
            ErrorKind::UnevenMap => "map literal must have an even number of values".to_owned(),
        }
    }

    fn span(&self) -> Span {
        self.span
    }

    fn level(&self) -> Level {
        Level::Error
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        if let ErrorKind::Eof(ref ec) = self.kind {
            if let Some(open_char_span) = ec.open_char_span() {
                return Some(Box::new(ContentStartHelp {
                    expected_content: *ec,
                    open_char_span,
                }));
            }
        }

        return None;
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
    fn description(&self) -> &'static str {
        match *self {
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

    fn open_char_span(&self) -> Option<Span> {
        match *self {
            ExpectedContent::List(span) => Some(span),
            ExpectedContent::Vector(span) => Some(span),
            ExpectedContent::Set(span) => Some(span),
            ExpectedContent::Map(span) => Some(span),
            ExpectedContent::String(span) => Some(span),
            _ => None,
        }
    }
}

struct ContentStartHelp {
    expected_content: ExpectedContent,
    open_char_span: Span,
}

impl Reportable for ContentStartHelp {
    fn level(&self) -> Level {
        Level::Help
    }

    fn span(&self) -> Span {
        self.open_char_span
    }

    fn message(&self) -> String {
        format!("{} starts here", self.expected_content.description())
    }
}
