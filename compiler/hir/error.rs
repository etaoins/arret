use std::{error, fmt, io, path, result};

use codespan_reporting::{Diagnostic, Label};

use arret_syntax::datum::DataStr;
use arret_syntax::error::Error as SyntaxError;
use arret_syntax::span::Span;

use crate::reporting::{diagnostic_for_syntax_error, LocTrace};

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    PrimRef,
    TyRef,
    MacroRef(DataStr),
    UnboundSym(DataStr),
    WrongArgCount(usize),
    IllegalArg(&'static str),
    NoMainFun,
    ExpectedSym,
    DefOutsideBody,
    ExportOutsideModule,
    NonDefInsideModule,
    ExportInsideRepl,
    PackageNotFound,
    ModuleNotFound(Box<path::Path>),
    NoMacroRule,
    DuplicateDef(Option<Span>, DataStr),
    MultipleZeroOrMoreMatch(Span),
    NoVecDestruc,
    ValueAsTy,
    UserError(DataStr),
    ReadError(Box<path::Path>),
    SyntaxError(SyntaxError),
    RustFunError(Box<str>),
}

#[derive(Debug, PartialEq)]
pub struct Error {
    loc_trace: LocTrace,
    kind: ErrorKind,
}

pub type Result<T, E = Error> = result::Result<T, E>;

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error {
            loc_trace: span.into(),
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
            loc_trace: self.loc_trace.with_macro_invocation(span),
            ..self
        }
    }

    pub fn message(&self) -> String {
        match self.kind() {
            ErrorKind::PrimRef => "cannot take the value of a primitive".to_owned(),
            ErrorKind::TyRef => "cannot take the value of a type".to_owned(),
            ErrorKind::MacroRef(ref sym) => format!("cannot take the value of macro: `{}`", sym),
            ErrorKind::UnboundSym(ref sym) => format!("unable to resolve symbol: `{}`", sym),
            ErrorKind::WrongArgCount(expected) => format!("wrong arg count; expected {}", expected),
            ErrorKind::IllegalArg(description) => (*description).to_owned(),
            ErrorKind::ExpectedSym => "expected symbol".to_owned(),
            ErrorKind::DefOutsideBody => "(def) outside module body".to_owned(),
            ErrorKind::DuplicateDef(_, ref sym) => format!("duplicate definition: `{}`", sym),
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
                "vectors can only be used in a destructure for type ascription in the form [name Type]".to_owned()
            }
            ErrorKind::ValueAsTy => "value cannot be used as a type".to_owned(),
            ErrorKind::UserError(ref message) => message.as_ref().to_owned(),
            ErrorKind::ReadError(ref filename) => format!("error reading `{}`", filename.to_string_lossy()),
            ErrorKind::SyntaxError(ref err) => err.kind().message(),
            ErrorKind::RustFunError(ref message) => message.clone().into_string(),
            ErrorKind::NoMainFun => "no main! function defined in root module".to_owned()
        }
    }
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Diagnostic {
        if let ErrorKind::SyntaxError(syntax_error) = error.kind() {
            // Just proxy this
            return diagnostic_for_syntax_error(syntax_error);
        }

        let diagnostic =
            Diagnostic::new_error(error.message()).with_labels(error.loc_trace.to_labels());

        // Add any secondary labels
        match error.kind {
            ErrorKind::DuplicateDef(Some(span), _) | ErrorKind::MultipleZeroOrMoreMatch(span) => {
                diagnostic
                    .with_label(Label::new_secondary(span).with_message("first definition here"))
            }
            _ => diagnostic,
        }
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message())
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
