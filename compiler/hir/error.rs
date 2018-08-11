use std::fmt::Display;
use std::{error, fmt, io, path, result};

use crate::reporting::{Level, Reportable};
use syntax::error::Error as SyntaxError;
use syntax::span::{Span, EMPTY_SPAN};

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

    fn with_macro_invocation_span(self, macro_invocation_span: Span) -> ErrorLoc {
        ErrorLoc {
            span: self.span,
            macro_invocation_span: Some(macro_invocation_span),
        }
    }
}

impl From<Span> for ErrorLoc {
    fn from(span: Span) -> ErrorLoc {
        ErrorLoc::new(span, None)
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    PrimRef,
    TyRef,
    MacroRef(Box<str>),
    UnboundSym(Box<str>),
    WrongArgCount(usize),
    IllegalArg(&'static str),
    ExpectedSym,
    DefOutsideBody,
    ExportOutsideModule,
    NonDefInsideModule,
    ExportInsideRepl,
    PackageNotFound,
    ModuleNotFound(Box<path::Path>),
    NoMacroRule,
    DuplicateDef(Span),
    MultipleZeroOrMoreMatch(Span),
    NoVecDestruc,
    ValueAsTy,
    UserError(Box<str>),
    ReadError(Box<path::Path>),
    SyntaxError(SyntaxError),
    RustFunError(Box<str>),
}

#[derive(Debug, PartialEq)]
pub struct Error {
    error_loc: ErrorLoc,
    kind: ErrorKind,
}

pub type Result<T, E = Error> = result::Result<T, E>;

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error {
            error_loc: span.into(),
            kind,
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn from_module_io(span: Span, path: &path::Path, error: &io::Error) -> Error {
        match error.kind() {
            io::ErrorKind::NotFound => Error::new(span, ErrorKind::ModuleNotFound(path.into())),
            _ => Error::new(span, ErrorKind::ReadError(path.into())),
        }
    }

    pub fn with_macro_invocation_span(self, span: Span) -> Error {
        Error {
            error_loc: self.error_loc.with_macro_invocation_span(span),
            kind: self.kind,
        }
    }

    pub(super) fn with_span_offset(self, offset: usize) -> Error {
        Error {
            error_loc: ErrorLoc {
                span: self.error_loc.span.with_offset(offset),
                macro_invocation_span: None,
            },
            kind: self.kind,
        }
    }
}

impl Reportable for Error {
    fn level(&self) -> Level {
        Level::Error
    }

    fn message(&self) -> String {
        match self.kind {
            ErrorKind::PrimRef => "cannot take the value of a primitive".to_owned(),
            ErrorKind::TyRef => "cannot take the value of a type".to_owned(),
            ErrorKind::MacroRef(ref sym) => format!("cannot take the value of macro: `{}`", sym),
            ErrorKind::UnboundSym(ref sym) => format!("unable to resolve symbol: `{}`", sym),
            ErrorKind::WrongArgCount(expected) => format!("wrong arg count; expected {}", expected),
            ErrorKind::IllegalArg(description) => description.to_owned(),
            ErrorKind::ExpectedSym => "expected symbol".to_owned(),
            ErrorKind::DefOutsideBody => "(def) outside module body".to_owned(),
            ErrorKind::DuplicateDef(_) => "duplicate definition".to_owned(),
            ErrorKind::ExportOutsideModule => "(export) outside of module body".to_owned(),
            ErrorKind::NonDefInsideModule => {
                "definition expected at the top-level of a module body".to_owned()
            }
            ErrorKind::ExportInsideRepl => {
                "export not supported within REPL".to_owned()
            }
            ErrorKind::PackageNotFound => "package not found".to_owned(),
            ErrorKind::ModuleNotFound(ref filename) => format!("module not found at `{}`", filename.to_string_lossy()).to_owned(),
            ErrorKind::NoMacroRule => "no matching macro rule".to_owned(),
            ErrorKind::MultipleZeroOrMoreMatch(_) => {
                "multiple zero or more matches in the same sequence".to_owned()
            }
            ErrorKind::NoVecDestruc => {
                "vectors can only be used in a destructure for type ascription in the form [name : Type]".to_owned()
            }
            ErrorKind::ValueAsTy => "value cannot be used as a type".to_owned(),
            ErrorKind::UserError(ref message) => message.clone().into_string(),
            ErrorKind::ReadError(ref filename) => format!("error reading `{}`", filename.to_string_lossy()),
            ErrorKind::SyntaxError(ref err) => err.message(),
            ErrorKind::RustFunError(ref message) => message.clone().into_string(),
        }
    }

    fn span(&self) -> Span {
        self.error_loc.span
    }

    fn macro_invocation_span(&self) -> Option<Span> {
        self.error_loc.macro_invocation_span
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        match self.kind {
            ErrorKind::DuplicateDef(span) => if span == EMPTY_SPAN {
                // Some definitions (e.g. `import`) are magically inserted in to the scope. They
                // won't have a span.
                None
            } else {
                Some(Box::new(FirstDefHelp { span }))
            },
            ErrorKind::MultipleZeroOrMoreMatch(span) => Some(Box::new(FirstDefHelp { span })),
            ErrorKind::SyntaxError(ref err) => err.associated_report(),
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
        Error::new(err.span(), ErrorKind::SyntaxError(err))
    }
}

impl From<Error> for Vec<Error> {
    fn from(error: Error) -> Vec<Error> {
        vec![error]
    }
}

struct FirstDefHelp {
    span: Span,
}

impl Reportable for FirstDefHelp {
    fn level(&self) -> Level {
        Level::Help
    }

    fn span(&self) -> Span {
        self.span
    }

    fn message(&self) -> String {
        "first definition here".to_owned()
    }
}
