pub mod destruc;
mod error;
mod import;
mod loader;
pub mod lowering;
mod macros;
mod module;
mod ns;
mod prim;
mod scope;
mod types;
mod util;

use std::ops::Range;

use syntax::datum::Datum;
use syntax::span::Span;
use ty;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VarId(u32);

impl VarId {
    fn new(id: u32) -> VarId {
        VarId(id)
    }
}

#[derive(PartialEq, Debug)]
pub struct Fun {
    pvar_ids: Range<ty::purity::PVarId>,
    tvar_ids: Range<ty::TVarId>,

    purity: ty::purity::Decl,
    params: destruc::List,
    ret_ty: ty::Decl,

    body_expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Cond {
    test_expr: Box<Expr>,
    true_expr: Box<Expr>,
    false_expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct App {
    fun_expr: Box<Expr>,
    fixed_arg_exprs: Vec<Expr>,
    rest_arg_expr: Option<Box<Expr>>,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Lit(Datum),
    App(Span, App),
    Fun(Span, Fun),
    Def(Span, destruc::Destruc, Box<Expr>),
    Cond(Span, Cond),
    Ref(Span, VarId),
    TyPred(Span, ty::Poly),
    Do(Vec<Expr>),
}

impl Expr {
    fn from_vec(exprs: Vec<Expr>) -> Expr {
        let mut flattened_exprs = vec![];

        for expr in exprs {
            match expr {
                Expr::Do(mut exprs) => {
                    flattened_exprs.append(&mut exprs);
                }
                other => {
                    flattened_exprs.push(other);
                }
            }
        }

        if flattened_exprs.len() == 1 {
            flattened_exprs.pop().unwrap()
        } else {
            Expr::Do(flattened_exprs)
        }
    }

    /// Returns the span for the expression
    ///
    /// This will return None for `Expr::Do` as it may not have a single contiguous span
    fn span(&self) -> Option<Span> {
        match self {
            Expr::Lit(datum) => Some(datum.span()),
            Expr::App(span, _)
            | Expr::Fun(span, _)
            | Expr::Def(span, _, _)
            | Expr::Cond(span, _)
            | Expr::Ref(span, _)
            | Expr::TyPred(span, _) => Some(*span),
            Expr::Do(_) => None,
        }
    }

    /// Returns the last expression
    ///
    /// This is the expression itself except for `Expr::Do` which will return the last expression
    fn last_expr(&self) -> Option<&Expr> {
        match self {
            Expr::Do(exprs) => exprs.last(),
            other => Some(other),
        }
    }
}

#[cfg(test)]
pub use self::types::poly_for_str;
