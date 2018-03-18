use std::{error, result};
use std::fmt;
use std::fmt::Display;

use syntax::span::Span;
use syntax::error::Error as SyntaxError;
use reporting::{Level, Reportable};

#[derive(Debug, PartialEq, Clone)]
pub struct ErrorLoc {
    span: Span,
    macro_invocation_span: Option<Span>,
}

impl ErrorLoc {
    pub fn new(span: Span, macro_invocation_span: Option<Span>) -> ErrorLoc {
        ErrorLoc {
            span,
            macro_invocation_span,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    PrimRef(ErrorLoc),
    MacroRef(ErrorLoc, String),
    UnboundSymbol(ErrorLoc, String),
    WrongArgCount(ErrorLoc, usize),
    IllegalArg(ErrorLoc, String),
    ExpectedSymbol(ErrorLoc),
    DefOutsideBody(ErrorLoc),
    ExportOutsideModule(ErrorLoc),
    LibraryNotFound(ErrorLoc),
    NoMacroRule(ErrorLoc),
    DuplicateMacroVar(ErrorLoc, String, Span),
    MultipleZeroOrMoreMatch(ErrorLoc, Span),
    ReadError(String),
    SyntaxError(SyntaxError),
}

pub type Result<T> = result::Result<T, Error>;

impl Error {
    fn error_loc(&self) -> Option<ErrorLoc> {
        match *self {
            Error::PrimRef(ref error_loc) => Some(error_loc.clone()),
            Error::MacroRef(ref error_loc, _) => Some(error_loc.clone()),
            Error::UnboundSymbol(ref error_loc, _) => Some(error_loc.clone()),
            Error::WrongArgCount(ref error_loc, _) => Some(error_loc.clone()),
            Error::IllegalArg(ref error_loc, _) => Some(error_loc.clone()),
            Error::ExpectedSymbol(ref error_loc) => Some(error_loc.clone()),
            Error::DefOutsideBody(ref error_loc) => Some(error_loc.clone()),
            Error::ExportOutsideModule(ref error_loc) => Some(error_loc.clone()),
            Error::LibraryNotFound(ref error_loc) => Some(error_loc.clone()),
            Error::NoMacroRule(ref error_loc) => Some(error_loc.clone()),
            Error::DuplicateMacroVar(ref error_loc, _, _) => Some(error_loc.clone()),
            Error::MultipleZeroOrMoreMatch(ref error_loc, _) => Some(error_loc.clone()),
            Error::ReadError(_) => None,
            Error::SyntaxError(ref err) => err.span().map(|span| ErrorLoc {
                span: span,
                macro_invocation_span: None,
            }),
        }
    }
}

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
        self.error_loc().map(|el| el.span)
    }

    fn macro_invocation_span(&self) -> Option<Span> {
        self.error_loc().and_then(|el| el.macro_invocation_span)
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

/// Wrapper for t2s that produces an ErrorLoc with no macro invocation span
#[cfg(test)]
pub fn t2el(v: &str) -> ErrorLoc {
    use syntax::span::t2s;
    ErrorLoc::new(t2s(v), None)
}
