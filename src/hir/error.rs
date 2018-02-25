use std::{error, result};
use std::fmt;
use std::fmt::Display;

use syntax::span::Span;
use syntax::error::Error as SyntaxError;
use reporting::{Level, Reportable};

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

impl Reportable for Error {
    fn level(&self) -> Level {
        Level::Error
    }

    fn message(&self) -> String {
        match *self {
            Error::PrimitiveRef(_, ref sym) => {
                format!("cannot take the value of a primitive: `{}`", sym)
            }
            Error::UnboundSymbol(_, ref sym) => format!("unable to resolve symbol: `{}`", sym),
            Error::SyntaxError(ref err) => err.message(),
            _ => "Lowering error".to_owned(),
        }
    }

    fn span(&self) -> Option<Span> {
        match *self {
            Error::PrimitiveRef(span, _) => Some(span),
            Error::UnboundSymbol(span, _) => Some(span),
            Error::SyntaxError(ref err) => err.span(),
            _ => None,
        }
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        match *self {
            Error::SyntaxError(ref err) => err.associated_report(),
            _ => None,
        }
    }
}

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
