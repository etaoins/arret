pub mod lowering;
mod scope;
mod error;
mod module;
mod loader;
mod prim;
mod ns;
mod macros;
mod types;
mod util;

use syntax::span::Span;
use syntax::datum::Datum;
use ty;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VarId(usize);

impl VarId {
    fn new(id: usize) -> VarId {
        VarId(id)
    }
}

#[derive(Debug)]
pub struct Var {
    id: VarId,
    source_name: String,
    bound: ty::Decl,
}

impl Var {
    fn with_bound(self, bound: ty::Poly) -> Var {
        Var {
            id: self.id,
            source_name: self.source_name,
            bound: bound.into_decl(),
        }
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        self.id.0 == other.id.0
    }
}

#[derive(Debug, PartialEq)]
pub enum Destruc {
    Var(Var),
    Wildcard(ty::Decl),
    List(Vec<Destruc>, Option<Box<Destruc>>),
}

#[derive(PartialEq, Debug)]
pub struct Fun {
    pvar_ids: Vec<ty::PVarId>,
    params: Destruc,
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
    Def(Span, Destruc, Box<Expr>),
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
        match *self {
            Expr::Lit(ref datum) => Some(datum.span()),
            Expr::App(span, _)
            | Expr::Fun(span, _)
            | Expr::Def(span, _, _)
            | Expr::Cond(span, _)
            | Expr::Ref(span, _)
            | Expr::TyPred(span, _) => Some(span),
            Expr::Do(_) => None,
        }
    }

    /// Returns the last expression
    ///
    /// This is the expression itself except for `Expr::Do` which will return the last expression
    fn last_expr(&self) -> Option<&Expr> {
        match self {
            &Expr::Do(ref exprs) => exprs.last(),
            other => Some(other),
        }
    }
}
