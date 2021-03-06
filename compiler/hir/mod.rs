pub(crate) mod destruc;
pub(crate) mod error;
pub(crate) mod exports;
pub(crate) mod import;
pub(crate) mod loader;
pub(crate) mod lowering;
mod macros;
pub(crate) mod ns;
mod prim;
mod records;
pub(crate) mod scope;
mod types;
mod util;
pub(crate) mod var_id;
pub(crate) mod visitor;

use std::sync::Arc;

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

use crate::rfi;
use crate::ty;
use crate::ty::purity;
use crate::ty::record;
use crate::ty::Ty;

pub use crate::hir::var_id::{ExportId, LocalId};

/// DeclTy is a type declared by a user
///
/// The `Known` variant indicates the type is specified while `Free` indicates it must be inferred.
#[derive(PartialEq, Debug, Clone)]
pub enum DeclTy {
    Known(ty::Ref<ty::Poly>),
    Free,
}

impl From<Ty<ty::Poly>> for DeclTy {
    fn from(ty: Ty<ty::Poly>) -> Self {
        DeclTy::Known(ty::Ref::Fixed(ty))
    }
}

impl From<ty::Ref<ty::Poly>> for DeclTy {
    fn from(poly: ty::Ref<ty::Poly>) -> Self {
        DeclTy::Known(poly)
    }
}

/// Decl is a purity declared by a user
///
/// The `Known` variant indicates the purity is specified while `Free` indicates it must be
/// inferred.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DeclPurity {
    Known(purity::Ref),
    Free,
}

impl From<purity::Ref> for DeclPurity {
    fn from(poly: purity::Ref) -> Self {
        DeclPurity::Known(poly)
    }
}

#[cfg(test)]
impl From<purity::Purity> for DeclPurity {
    fn from(purity: purity::Purity) -> Self {
        DeclPurity::Known(purity::Ref::Fixed(purity))
    }
}

pub trait Phase: Clone + std::cmp::PartialEq + std::fmt::Debug {
    type Purity: Clone + std::cmp::PartialEq + std::fmt::Debug;
    type DeclType: Clone + std::cmp::PartialEq + std::fmt::Debug;
    type ResultType: Clone + std::cmp::PartialEq + std::fmt::Debug;
    type TyArgs: Clone + std::cmp::PartialEq + std::fmt::Debug;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Inferred {}
impl Phase for Inferred {
    type Purity = purity::Ref;
    type DeclType = ty::Ref<ty::Poly>;
    type ResultType = ty::Ref<ty::Poly>;
    type TyArgs = ty::ty_args::TyArgs<ty::Poly>;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Lowered {}
impl Phase for Lowered {
    type Purity = DeclPurity;
    type DeclType = DeclTy;
    type ResultType = ();
    type TyArgs = ();
}

#[derive(PartialEq, Debug, Clone)]
pub struct Fun<P: Phase> {
    pub span: Span,

    pub pvars: purity::PVars,
    pub tvars: ty::TVars,

    pub purity: P::Purity,
    pub params: destruc::List<P>,

    pub ret_ty: P::DeclType,
    pub ret_ty_span: Option<Span>,

    pub body_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cond<P: Phase> {
    pub span: Span,
    pub test_expr: Expr<P>,
    pub true_expr: Expr<P>,
    pub false_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Let<P: Phase> {
    pub span: Span,
    pub destruc: destruc::Destruc<P>,
    pub value_expr: Expr<P>,
    pub body_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct App<P: Phase> {
    pub span: Span,
    pub fun_expr: Expr<P>,
    pub ty_args: P::TyArgs,
    pub fixed_arg_exprs: Vec<Expr<P>>,
    pub rest_arg_expr: Option<Expr<P>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Recur<P: Phase> {
    pub span: Span,
    pub fixed_arg_exprs: Vec<Expr<P>>,
    pub rest_arg_expr: Option<Expr<P>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FieldAccessor {
    pub span: Span,
    pub record_cons: record::ConsId,
    pub field_index: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Expr<P: Phase> {
    pub result_ty: P::ResultType,
    pub kind: ExprKind<P>,
}

impl From<Datum> for Expr<Lowered> {
    fn from(datum: Datum) -> Expr<Lowered> {
        ExprKind::Lit(datum).into()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExprKind<P: Phase> {
    Lit(Datum),
    App(Box<App<P>>),
    Recur(Box<Recur<P>>),
    Fun(Box<Fun<P>>),
    RustFun(Arc<rfi::Fun>),
    Let(Box<Let<P>>),
    Cond(Box<Cond<P>>),
    ExportRef(Span, ExportId),
    LocalRef(Span, LocalId),

    TyPred(Span, ty::pred::TestTy),
    EqPred(Span),
    RecordCons(Span, record::ConsId),
    FieldAccessor(Box<FieldAccessor>),

    Do(Vec<Expr<P>>),

    /// Used for tracing macro expansion for error report and debug information
    ///
    /// Other than the above this should be treated identically to the inner expression.
    MacroExpand(Span, Box<Expr<P>>),
}

impl From<ExprKind<Lowered>> for Expr<Lowered> {
    fn from(kind: ExprKind<Lowered>) -> Expr<Lowered> {
        Expr {
            result_ty: (),
            kind,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Def<P: Phase> {
    pub span: Span,
    pub macro_invocation_span: Option<Span>,
    pub destruc: destruc::Destruc<P>,
    pub value_expr: Expr<P>,
}

pub use self::loader::PackagePaths;
pub use self::types::lower_poly;
pub use self::types::str_for_purity;
pub use self::types::str_for_ty_ref;

#[cfg(test)]
pub use self::types::{lower_polymorphic_var_set, poly_for_str, try_lower_purity, tvar_bounded_by};

#[cfg(test)]
pub use self::lowering::expr_for_str;
