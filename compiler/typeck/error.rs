use std::error;
use std::fmt;
use std::fmt::Display;

use crate::reporting::{Level, LocTrace, Reportable};
use syntax::span::Span;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct WantedArity {
    fixed_len: usize,
    has_rest: bool,
}

impl WantedArity {
    pub fn new(fixed_len: usize, has_rest: bool) -> WantedArity {
        WantedArity {
            fixed_len,
            has_rest,
        }
    }
}

impl fmt::Display for WantedArity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_rest {
            write!(f, "at least {}", self.fixed_len)
        } else {
            write!(f, "{}", self.fixed_len)
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ErrorKind {
    IsNotTy(Box<str>, Box<str>),
    IsNotFun(Box<str>),
    IsNotPurity(Box<str>, Box<str>),
    VarHasEmptyType(Box<str>, Box<str>),
    TopFunApply(Box<str>),
    RecursiveType,
    DependsOnError,
    WrongArity(usize, WantedArity),
}

#[derive(PartialEq, Debug)]
pub struct Error {
    loc_trace: LocTrace,
    kind: ErrorKind,
}

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

impl Reportable for Error {
    fn level(&self) -> Level {
        Level::Error
    }

    fn message(&self) -> String {
        match self.kind() {
            ErrorKind::IsNotFun(ref sub) => format!("`{}` is not a function", sub),
            ErrorKind::IsNotTy(ref sub, ref parent) => format!("`{}` is not a `{}`", sub, parent),
            ErrorKind::IsNotPurity(ref fun, ref purity) => {
                format!("function of type `{}` is not {}", fun, purity)
            }
            ErrorKind::VarHasEmptyType(ref left, ref right) => {
                format!("inferred conflicting types `{}` and `{}`", left, right)
            }
            ErrorKind::TopFunApply(ref top_fun) => format!(
                "cannot determine parameter types for top function type `{}`",
                top_fun
            ),
            ErrorKind::WrongArity(have, ref wanted) => format!(
                "incorrect number of arguments: wanted {}, have {}",
                wanted, have
            ),
            ErrorKind::RecursiveType => {
                "recursive usage requires explicit type annotation".to_owned()
            }
            ErrorKind::DependsOnError => {
                "type cannot be determined due to previous error".to_owned()
            }
        }
    }

    fn loc_trace(&self) -> LocTrace {
        self.loc_trace.clone()
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "Type check error"
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
