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

pub trait HirType: Clone + std::cmp::PartialEq + std::fmt::Debug {
    type Purity: Clone + std::cmp::PartialEq + std::fmt::Debug;
}

impl HirType for ty::Poly {
    type Purity = purity::Poly;
}

impl HirType for ty::Decl {
    type Purity = purity::Decl;
}

new_global_id_type!(VarId);

#[derive(PartialEq, Debug, Clone)]
pub struct Fun<T: HirType> {
    pub pvars: purity::PVars,
    pub tvars: ty::TVars,

    pub purity: T::Purity,
    pub params: destruc::List<T>,
    pub ret_ty: T,

    pub body_expr: Expr<T>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cond<T: HirType> {
    pub test_expr: Expr<T>,
    pub true_expr: Expr<T>,
    pub false_expr: Expr<T>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Let<T: HirType> {
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Expr<T>,
    pub body_expr: Expr<T>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct App<T: HirType> {
    pub fun_expr: Expr<T>,
    pub fixed_arg_exprs: Vec<Expr<T>>,
    pub rest_arg_expr: Option<Expr<T>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<T: HirType> {
    Lit(Datum),
    App(Span, Box<App<T>>),
    Fun(Span, Box<Fun<T>>),
    RustFun(Span, Box<rfi::Fun>),
    Let(Span, Box<Let<T>>),
    Cond(Span, Box<Cond<T>>),
    Ref(Span, VarId),
    TyPred(Span, ty::Poly),
    Do(Vec<Expr<T>>),

    /// Used for tracing macro expansion for error report and debug information
    ///
    /// Other than the above this should be treated identically to the inner expression.
    MacroExpand(Span, Box<Expr<T>>),
}

#[derive(PartialEq, Debug)]
pub struct Def<T: HirType> {
    pub span: Span,
    pub macro_invocation_span: Span,
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Expr<T>,
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
