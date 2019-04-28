use std::{error, fmt, result};

use arret_syntax::span::Span;

use crate::mir::inliner::ApplyCookie;
use crate::reporting::{LocTrace, Reportable, Severity};

#[derive(Debug, PartialEq)]
pub struct Panic {
    loc_trace: LocTrace,
    message: String,
}

impl Panic {
    pub fn new(span: Span, message: String) -> Panic {
        Panic {
            loc_trace: span.into(),
            message,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Panic(Panic),
    /// Internal error used to abort a recursive function application when a loop is detected
    AbortRecursion(ApplyCookie),
    /// Internal error indicating that a divergent value was encountered
    Diverged,
}

pub type Result<T> = result::Result<T, Error>;

impl Error {
    pub fn with_macro_invocation_span(self, span: Span) -> Error {
        match self {
            Error::Panic(Panic { loc_trace, message }) => Error::Panic(Panic {
                loc_trace: loc_trace.with_macro_invocation(span),
                message,
            }),
            other => other,
        }
    }
}

impl error::Error for Panic {
    fn description(&self) -> &str {
        "Panic during HIR evaluation"
    }
}

impl fmt::Display for Panic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Reportable for Panic {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self) -> String {
        self.message.clone()
    }

    fn loc_trace(&self) -> LocTrace {
        self.loc_trace.clone()
    }
}
