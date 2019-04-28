use std::error;
use std::fmt;
use std::fmt::Display;

use arret_syntax::span::Span;

use crate::hir;
use crate::reporting::{LocTrace, Reportable, Severity};
use crate::ty;
use crate::ty::purity;

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
    IsNotTy(ty::Ref<ty::Poly>, ty::Ref<ty::Poly>),
    IsNotFun(ty::Ref<ty::Poly>),
    IsNotPurity(ty::Ref<ty::Poly>, purity::Ref),
    VarHasEmptyType(ty::Ref<ty::Poly>, ty::Ref<ty::Poly>),
    TopFunApply(ty::Ref<ty::Poly>),
    RecursiveType,
    DependsOnError,
    WrongArity(usize, WantedArity),
    UnselectedPVar(purity::PVarId),
    UnselectedTVar(ty::TVarId),
}

#[derive(PartialEq, Debug)]
pub struct Error {
    loc_trace: LocTrace,
    kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Self::new_with_loc_trace(span.into(), kind)
    }

    pub fn new_with_loc_trace(loc_trace: LocTrace, kind: ErrorKind) -> Error {
        Error { loc_trace, kind }
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
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self) -> String {
        match self.kind() {
            ErrorKind::IsNotFun(ref sub) => {
                format!("`{}` is not a function", hir::str_for_ty_ref(sub))
            }
            ErrorKind::IsNotTy(ref sub, ref parent) => format!(
                "`{}` is not a `{}`",
                hir::str_for_ty_ref(sub),
                hir::str_for_ty_ref(parent)
            ),
            ErrorKind::IsNotPurity(ref fun, ref purity) => {
                use crate::ty::purity::Purity;

                let purity_str = if purity == &Purity::Pure.into() {
                    // `->` might be confusing here
                    "pure".into()
                } else {
                    format!("`{}`", hir::str_for_purity(purity))
                };

                format!(
                    "function of type `{}` is not {}",
                    hir::str_for_ty_ref(fun),
                    purity_str
                )
            }
            ErrorKind::VarHasEmptyType(ref left, ref right) => format!(
                "inferred conflicting types `{}` and `{}`",
                hir::str_for_ty_ref(left),
                hir::str_for_ty_ref(right)
            ),
            ErrorKind::TopFunApply(ref top_fun) => format!(
                "cannot determine parameter types for top function type `{}`",
                hir::str_for_ty_ref(top_fun)
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
            ErrorKind::UnselectedPVar(pvar_id) => format!(
                "cannot determine purity of purity variable `{}` in this context",
                pvar_id.source_name()
            ),
            ErrorKind::UnselectedTVar(pvar_id) => format!(
                "cannot determine type of type variable `{}` in this context",
                pvar_id.source_name()
            ),
        }
    }

    fn loc_trace(&self) -> LocTrace {
        self.loc_trace.clone()
    }

    fn associated_report(&self) -> Option<Box<dyn Reportable>> {
        match self.kind() {
            ErrorKind::UnselectedPVar(pvar_id) => Some(Box::new(PVarDefHelp {
                span: pvar_id.span(),
            })),
            ErrorKind::UnselectedTVar(tvar_id) => Some(Box::new(TVarDefHelp {
                span: tvar_id.span(),
            })),
            _ => None,
        }
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

struct PVarDefHelp {
    span: Span,
}

impl Reportable for PVarDefHelp {
    fn severity(&self) -> Severity {
        Severity::Help
    }

    fn loc_trace(&self) -> LocTrace {
        self.span.into()
    }

    fn message(&self) -> String {
        "purity variable defined here".to_owned()
    }
}

struct TVarDefHelp {
    span: Span,
}

impl Reportable for TVarDefHelp {
    fn severity(&self) -> Severity {
        Severity::Help
    }

    fn loc_trace(&self) -> LocTrace {
        self.span.into()
    }

    fn message(&self) -> String {
        "type variable defined here".to_owned()
    }
}
