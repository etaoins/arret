use std::{error, fmt, result};

use syntax::span::Span;

use crate::reporting::{Level, LocTrace, Reportable};

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Panic(String),
}

#[derive(Debug, PartialEq)]
pub struct Error {
    loc_trace: LocTrace,
    kind: ErrorKind,
}

pub type Result<T> = result::Result<T, Error>;

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

    pub fn with_macro_invocation_span(self, span: Span) -> Error {
        Error {
            loc_trace: self.loc_trace.with_macro_invocation(span),
            ..self
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "HIR evaluation error"
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Reportable for Error {
    fn level(&self) -> Level {
        Level::Error
    }

    fn message(&self) -> String {
        match self.kind {
            ErrorKind::Panic(ref message) => message.clone(),
        }
    }

    fn loc_trace(&self) -> LocTrace {
        self.loc_trace.clone()
    }
}
