use std::{error, result};
use std::fmt;
use std::fmt::Display;

use syntax::span::Span;
use syntax::error::Error as SyntaxError;
use reporting::{Level, Reportable};

#[derive(Debug, PartialEq)]
pub enum Error {
    PrimRef(Span),
    MacroRef(Span, String),
    UnboundSymbol(Span, String),
    WrongArgCount(Span, usize),
    IllegalArg(Span, String),
    ExpectedSymbol(Span),
    DefOutsideBody(Span),
    ExportOutsideModule(Span),
    LibraryNotFound(Span),
    NoMacroRule(Span),
    DuplicateMacroVar(Span, String, Span),
    MultipleZeroOrMoreMatch(Span, Span),
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
            Error::PrimRef(_) => "cannot take the value of a primitive".to_owned(),
            Error::MacroRef(_, ref sym) => format!("cannot take the value of macro: `{}`", sym),
            Error::UnboundSymbol(_, ref sym) => format!("unable to resolve symbol: `{}`", sym),
            Error::WrongArgCount(_, expected) => format!("wrong arg count; expected {}", expected),
            Error::IllegalArg(_, ref description) => format!("illegal argument: {}", description),
            Error::ExpectedSymbol(_) => "expected symbol".to_owned(),
            Error::DefOutsideBody(_) => "(def) outside module or function body".to_owned(),
            Error::ExportOutsideModule(_) => "(export) outside of module body".to_owned(),
            Error::LibraryNotFound(_) => "library not found".to_owned(),
            Error::NoMacroRule(_) => "no matching macro rule".to_owned(),
            Error::DuplicateMacroVar(_, ref sym, _) => {
                format!("duplicate macro variable: `{}`", sym)
            }
            Error::MultipleZeroOrMoreMatch(_, _) => {
                "Multiple zero or more matches in the same sequence".to_owned()
            }
            Error::ReadError(ref filename) => format!("error reading `{}`", filename),
            Error::SyntaxError(ref err) => err.message(),
        }
    }

    fn span(&self) -> Option<Span> {
        match *self {
            Error::PrimRef(span) => Some(span),
            Error::MacroRef(span, _) => Some(span),
            Error::UnboundSymbol(span, _) => Some(span),
            Error::WrongArgCount(span, _) => Some(span),
            Error::IllegalArg(span, _) => Some(span),
            Error::ExpectedSymbol(span) => Some(span),
            Error::DefOutsideBody(span) => Some(span),
            Error::ExportOutsideModule(span) => Some(span),
            Error::LibraryNotFound(span) => Some(span),
            Error::NoMacroRule(span) => Some(span),
            Error::DuplicateMacroVar(span, _, _) => Some(span),
            Error::MultipleZeroOrMoreMatch(span, _) => Some(span),
            Error::ReadError(_) => None,
            Error::SyntaxError(ref err) => err.span(),
        }
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        match *self {
            Error::DuplicateMacroVar(_, _, span) => Some(Box::new(FirstDefHelp { span })),
            Error::MultipleZeroOrMoreMatch(_, span) => Some(Box::new(FirstDefHelp { span })),
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

struct FirstDefHelp {
    span: Span,
}

impl Reportable for FirstDefHelp {
    fn level(&self) -> Level {
        Level::Help
    }

    fn span(&self) -> Option<Span> {
        Some(self.span)
    }

    fn message(&self) -> String {
        "first definition here".to_owned()
    }
}
