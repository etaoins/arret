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

use std;
use std::ops::Range;

use crate::ty;
use syntax::datum::Datum;
use syntax::span::Span;

pub trait HirType: std::cmp::PartialEq + std::fmt::Debug {
    type Purity: std::cmp::PartialEq + std::fmt::Debug;
}

impl HirType for ty::Poly {
    type Purity = ty::purity::Poly;
}

impl HirType for ty::Decl {
    type Purity = ty::purity::Decl;
}

new_counting_id_type!(VarIdCounter, VarId, u32);

#[derive(PartialEq, Debug)]
pub struct Fun<T: HirType> {
    pub pvar_ids: Range<ty::purity::PVarId>,
    pub tvar_ids: Range<ty::TVarId>,

    pub purity: T::Purity,
    pub params: destruc::List<T>,
    pub ret_ty: T,

    pub body_expr: Expr<T>,
}

#[derive(PartialEq, Debug)]
pub struct Cond<T: HirType> {
    pub test_expr: Expr<T>,
    pub true_expr: Expr<T>,
    pub false_expr: Expr<T>,
}

#[derive(PartialEq, Debug)]
pub struct Let<T: HirType> {
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Expr<T>,
    pub body_expr: Expr<T>,
}

#[derive(PartialEq, Debug)]
pub struct App<T: HirType> {
    pub fun_expr: Expr<T>,
    pub fixed_arg_exprs: Vec<Expr<T>>,
    pub rest_arg_expr: Option<Expr<T>>,
}

#[derive(PartialEq, Debug)]
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
}

#[derive(PartialEq, Debug)]
pub struct Def<T: HirType> {
    pub span: Span,
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Expr<T>,
}

pub use self::loader::PackagePaths;
pub use self::types::str_for_poly;
pub use self::types::str_for_purity;

#[cfg(test)]
pub use self::types::poly_for_str;

#[cfg(test)]
pub use self::lowering::expr_for_str;
