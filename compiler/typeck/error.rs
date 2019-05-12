use std::error;
use std::fmt;
use std::fmt::Display;

use codespan_reporting::{Diagnostic, Label};

use arret_syntax::span::Span;

use crate::hir;
use crate::reporting::LocTrace;
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

#[derive(PartialEq, Debug, Clone)]
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

#[derive(PartialEq, Debug, Clone)]
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

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Diagnostic {
        let origin = error.loc_trace.origin();

        let diagnostic = match error.kind() {
            ErrorKind::IsNotFun(ref sub) => Diagnostic::new_error(format!(
                "expected function, found `{}`",
                hir::str_for_ty_ref(sub)
            ))
            .with_label(Label::new_primary(origin).with_message("application requires function")),

            ErrorKind::IsNotTy(ref sub, ref parent) => Diagnostic::new_error("mismatched types")
                .with_label(Label::new_primary(origin).with_message(format!(
                    "`{}` is not a `{}`",
                    hir::str_for_ty_ref(sub),
                    hir::str_for_ty_ref(parent)
                ))),

            ErrorKind::IsNotPurity(ref fun, ref purity) => {
                use crate::ty::purity::Purity;

                let purity_str = if purity == &Purity::Pure.into() {
                    // `->` might be confusing here
                    "pure".into()
                } else {
                    format!("`{}`", hir::str_for_purity(purity))
                };

                Diagnostic::new_error("mismatched purities").with_label(
                    Label::new_primary(origin).with_message(format!(
                        "function of type `{}` is not {}",
                        hir::str_for_ty_ref(fun),
                        purity_str
                    )),
                )
            }

            ErrorKind::VarHasEmptyType(ref left, ref right) => Diagnostic::new_error(
                "type annotation needed",
            )
            .with_label(Label::new_primary(origin).with_message(format!(
                "inferred conflicting types `{}` and `{}`",
                hir::str_for_ty_ref(left),
                hir::str_for_ty_ref(right)
            ))),

            ErrorKind::TopFunApply(ref top_fun) => Diagnostic::new_error(format!(
                "cannot determine parameter types for `{}`",
                hir::str_for_ty_ref(top_fun)
            ))
            .with_label(Label::new_primary(origin).with_message("at this application")),

            ErrorKind::WrongArity(have, ref wanted) => {
                let label_message = if wanted.fixed_len == 1 {
                    format!("expected {} argument", wanted)
                } else {
                    format!("expected {} arguments", wanted)
                };

                Diagnostic::new_error(format!(
                    "incorrect number of arguments: wanted {}, have {}",
                    wanted, have
                ))
                .with_label(Label::new_primary(origin).with_message(label_message))
            }

            ErrorKind::RecursiveType => Diagnostic::new_error("type annotation needed").with_label(
                Label::new_primary(origin)
                    .with_message("recursive usage requires explicit type annotation".to_owned()),
            ),

            ErrorKind::DependsOnError => {
                Diagnostic::new_error("type cannot be determined due to previous error")
                    .with_label(Label::new_primary(origin))
            }

            ErrorKind::UnselectedPVar(pvar_id) => Diagnostic::new_error(format!(
                "cannot determine purity of purity variable `{}`",
                pvar_id.source_name()
            ))
            .with_label(Label::new_primary(origin).with_message("at this application"))
            .with_label(
                Label::new_secondary(pvar_id.span()).with_message("purity variable defined here"),
            ),

            ErrorKind::UnselectedTVar(tvar_id) => Diagnostic::new_error(format!(
                "cannot determine type of type variable `{}`",
                tvar_id.source_name()
            ))
            .with_label(Label::new_primary(origin).with_message("at this application"))
            .with_label(
                Label::new_secondary(tvar_id.span()).with_message("type variable defined here"),
            ),
        };

        error.loc_trace.label_macro_invocation(diagnostic)
    }
}

impl From<Error> for Vec<Diagnostic> {
    fn from(error: Error) -> Vec<Diagnostic> {
        vec![error.into()]
    }
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let diagnostic: Diagnostic = self.clone().into();
        f.write_str(&diagnostic.message)
    }
}
