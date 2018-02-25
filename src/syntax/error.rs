use std::result;
use std::fmt;
use std::fmt::Display;
use std::error;

use syntax::span::Span;
use reporting::{Level, Reportable};

#[derive(Debug, PartialEq)]
pub enum Error {
    Eof(Span, ExpectedContent),
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
            Error::Eof(_, ref ec) => {
                format!("unexpected end of file while parsing {}", ec.description())
            }
            Error::UnexpectedChar(_, c) => format!("unexpected `{}`", c),
            _ => "syntax error".to_owned(),
        }
    }

    fn span(&self) -> Option<Span> {
        match *self {
            Error::Eof(span, _) => Some(span),
            Error::UnexpectedChar(span, _) => Some(span),
            _ => None,
        }
    }

    fn level(&self) -> Level {
        Level::Error
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        if let Error::Eof(_, ref ec) = *self {
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

pub struct ContentStartHelp {
    expected_content: ExpectedContent,
    open_char_span: Span,
}

impl Reportable for ContentStartHelp {
    fn level(&self) -> Level {
        Level::Help
    }

    fn span(&self) -> Option<Span> {
        Some(self.open_char_span)
    }

    fn message(&self) -> String {
        format!("{} starts here", self.expected_content.description())
    }
}
