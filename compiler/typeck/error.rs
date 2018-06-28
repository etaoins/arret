use std::error;
use std::fmt;
use std::fmt::Display;

use reporting::{Level, Reportable};
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    IsNotPurity(Box<str>, Box<str>),
    VarHasEmptyType(Box<str>, Box<str>),
    TopFunApply(Box<str>),
    RecursiveType,
    DependsOnError,
    TooManyArgs(usize, WantedArity),
    InsufficientArgs(usize, WantedArity),
}

#[derive(PartialEq, Debug)]
pub struct Error(Span, ErrorKind);

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error(span, kind)
    }

    pub fn span(&self) -> Span {
        self.0
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.1
    }
}

impl Reportable for Error {
    fn level(&self) -> Level {
        Level::Error
    }

    fn message(&self) -> String {
        match self.1 {
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
            ErrorKind::TooManyArgs(have, ref wanted) => {
                format!("too many arguments: wanted {}, have {}", wanted, have)
            }
            ErrorKind::InsufficientArgs(have, wanted) => {
                format!("insufficient arguments: wanted {}, have {}", wanted, have)
            }
            ErrorKind::RecursiveType => {
                "recursive usage requires explicit type annotation".to_owned()
            }
            ErrorKind::DependsOnError => {
                "type cannot be determined due to previous error".to_owned()
            }
        }
    }

    fn span(&self) -> Span {
        self.span()
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "Type check error"
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
