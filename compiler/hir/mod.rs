pub(crate) mod destruc;
pub(crate) mod error;
mod exports;
mod import;
mod loader;
pub(crate) mod lowering;
mod macros;
pub(crate) mod ns;
mod prim;
pub(crate) mod rfi;
pub(crate) mod scope;
mod types;
mod util;
pub(crate) mod visitor;

use std;

use syntax::datum::Datum;
use syntax::span::Span;

use crate::ty;
use crate::ty::purity;

pub trait Phase: Clone + std::cmp::PartialEq + std::fmt::Debug {
    type Purity: Clone + std::cmp::PartialEq + std::fmt::Debug;
    type DeclType: Clone + std::cmp::PartialEq + std::fmt::Debug;
    type ResultType: Clone + std::cmp::PartialEq + std::fmt::Debug;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Inferred {}
impl Phase for Inferred {
    type Purity = purity::Poly;
    type DeclType = ty::Poly;
    type ResultType = ty::Poly;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Lowered {}
impl Phase for Lowered {
    type Purity = purity::Decl;
    type DeclType = ty::Decl;
    type ResultType = ();
}

new_global_id_type!(VarId);

#[derive(PartialEq, Debug, Clone)]
pub struct Fun<P: Phase> {
    pub pvars: purity::PVars,
    pub tvars: ty::TVars,

    pub purity: P::Purity,
    pub params: destruc::List<P>,
    pub ret_ty: P::DeclType,

    pub body_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cond<P: Phase> {
    pub phi_ty: P::ResultType,
    pub test_expr: Expr<P>,
    pub true_expr: Expr<P>,
    pub false_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Let<P: Phase> {
    pub destruc: destruc::Destruc<P>,
    pub value_expr: Expr<P>,
    pub body_expr: Expr<P>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct App<P: Phase> {
    pub ret_ty: P::ResultType,
    pub fun_expr: Expr<P>,
    pub fixed_arg_exprs: Vec<Expr<P>>,
    pub rest_arg_expr: Option<Expr<P>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<P: Phase> {
    Lit(Datum),
    App(Span, Box<App<P>>),
    Fun(Span, Box<Fun<P>>),
    RustFun(Span, Box<rfi::Fun>),
    Let(Span, Box<Let<P>>),
    Cond(Span, Box<Cond<P>>),
    Ref(Span, VarId),
    TyPred(Span, ty::pred::TestTy),
    Do(Vec<Expr<P>>),

    /// Used for tracing macro expansion for error report and debug information
    ///
    /// Other than the above this should be treated identically to the inner expression.
    MacroExpand(Span, Box<Expr<P>>),
}

#[derive(PartialEq, Debug)]
pub struct Def<P: Phase> {
    pub span: Span,
    pub macro_invocation_span: Span,
    pub destruc: destruc::Destruc<P>,
    pub value_expr: Expr<P>,
}

pub use self::loader::PackagePaths;
pub use self::types::str_for_poly;
pub use self::types::str_for_purity;

#[cfg(test)]
pub use self::types::poly_for_str;

#[cfg(test)]
pub use self::types::lower_polymorphic_vars;

#[cfg(test)]
pub use self::types::lower_poly;

#[cfg(test)]
pub use self::types::try_lower_purity;

#[cfg(test)]
pub use self::lowering::expr_for_str;
