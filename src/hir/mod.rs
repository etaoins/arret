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

impl Fun {
    pub fn pvar_ids(&self) -> &Range<ty::purity::PVarId> {
        &self.pvar_ids
    }

    pub fn tvar_ids(&self) -> &Range<ty::TVarId> {
        &self.tvar_ids
    }

    pub fn purity(&self) -> &ty::purity::Decl {
        &self.purity
    }

    pub fn params(&self) -> &destruc::List {
        &self.params
    }

    pub fn ret_ty(&self) -> &ty::Decl {
        &self.ret_ty
    }

    pub fn body_expr(&self) -> &Expr {
        &self.body_expr
    }
}

#[derive(PartialEq, Debug)]
pub struct Cond {
    test_expr: Box<Expr>,
    true_expr: Box<Expr>,
    false_expr: Box<Expr>,
}

impl Cond {
    pub fn test_expr(&self) -> &Expr {
        &self.test_expr
    }

    pub fn true_expr(&self) -> &Expr {
        &self.true_expr
    }

    pub fn false_expr(&self) -> &Expr {
        &self.false_expr
    }
}

#[derive(PartialEq, Debug)]
pub struct Let {
    destruc: destruc::Destruc,
    value_expr: Box<Expr>,
    body_expr: Box<Expr>,
}

impl Let {
    pub fn destruc(&self) -> &destruc::Destruc {
        &self.destruc
    }

    pub fn value_expr(&self) -> &Expr {
        &self.value_expr
    }

    pub fn body_expr(&self) -> &Expr {
        &self.body_expr
    }
}

#[derive(PartialEq, Debug)]
pub struct App {
    fun_expr: Box<Expr>,
    fixed_arg_exprs: Vec<Expr>,
    rest_arg_expr: Option<Box<Expr>>,
}

impl App {
    pub fn fun_expr(&self) -> &Expr {
        &self.fun_expr
    }

    pub fn fixed_arg_exprs(&self) -> &[Expr] {
        self.fixed_arg_exprs.as_ref()
    }

    pub fn rest_arg_expr(&self) -> &Option<Box<Expr>> {
        &self.rest_arg_expr
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Lit(Datum),
    App(Span, App),
    Fun(Span, Fun),
    Let(Span, Let),
    Cond(Span, Cond),
    Ref(Span, VarId),
    TyPred(Span, ty::Poly),
    Do(Vec<Expr>),
}

impl Expr {
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

#[derive(PartialEq, Debug)]
pub struct Def {
    span: Span,
    destruc: destruc::Destruc,
    value_expr: Expr,
}

impl Def {
    pub fn new(span: Span, destruc: destruc::Destruc, value_expr: Expr) -> Def {
        Def {
            span,
            destruc,
            value_expr,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn destruc(&self) -> &destruc::Destruc {
        &self.destruc
    }

    pub fn value_expr(&self) -> &Expr {
        &self.value_expr
    }
}

pub use self::types::{str_for_poly, str_for_purity};

#[cfg(test)]
pub use self::types::poly_for_str;

#[cfg(test)]
pub use self::lowering::expr_for_str;
