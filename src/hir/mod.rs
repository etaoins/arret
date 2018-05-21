pub mod destruc;
mod error;
mod import;
mod loader;
pub mod lowering;
mod macros;
pub mod module;
mod ns;
mod prim;
mod scope;
mod types;
mod util;

use std;
use std::ops::Range;

use syntax::datum::Datum;
use syntax::span::Span;
use ty;

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
pub struct Fun<T>
where
    T: HirType,
{
    pub pvar_ids: Range<ty::purity::PVarId>,
    pub tvar_ids: Range<ty::TVarId>,

    pub purity: T::Purity,
    pub params: destruc::List<T>,
    pub ret_ty: T,

    pub body_expr: Box<Expr<T>>,
}

#[derive(PartialEq, Debug)]
pub struct Cond<T>
where
    T: HirType,
{
    pub test_expr: Box<Expr<T>>,
    pub true_expr: Box<Expr<T>>,
    pub false_expr: Box<Expr<T>>,
}

#[derive(PartialEq, Debug)]
pub struct Let<T>
where
    T: HirType,
{
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Box<Expr<T>>,
    pub body_expr: Box<Expr<T>>,
}

#[derive(PartialEq, Debug)]
pub struct App<T>
where
    T: HirType,
{
    pub fun_expr: Box<Expr<T>>,
    pub fixed_arg_exprs: Vec<Expr<T>>,
    pub rest_arg_expr: Option<Box<Expr<T>>>,
}

#[derive(PartialEq, Debug)]
pub enum Expr<T>
where
    T: HirType,
{
    Lit(Datum),
    App(Span, App<T>),
    Fun(Span, Fun<T>),
    Let(Span, Let<T>),
    Cond(Span, Cond<T>),
    Ref(Span, VarId),
    TyPred(Span, ty::Poly),
    Do(Vec<Expr<T>>),
}

impl<T> Expr<T>
where
    T: HirType,
{
    /// Returns the span for the expression
    ///
    /// This will return None for `Expr::Do` as it may not have a single contiguous span
    pub fn span(&self) -> Option<Span> {
        match self {
            Expr::Lit(datum) => Some(datum.span()),
            Expr::App(span, _)
            | Expr::Fun(span, _)
            | Expr::Let(span, _)
            | Expr::Cond(span, _)
            | Expr::Ref(span, _)
            | Expr::TyPred(span, _) => Some(*span),
            Expr::Do(_) => None,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Def<T>
where
    T: HirType,
{
    pub span: Span,
    pub destruc: destruc::Destruc<T>,
    pub value_expr: Expr<T>,
}

pub use self::types::{str_for_poly, str_for_purity};

#[cfg(test)]
pub use self::types::poly_for_str;

#[cfg(test)]
pub use self::lowering::expr_for_str;
