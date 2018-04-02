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
    bound: Option<ty::Poly>,
}

impl Var {
    fn with_bound(self, bound: ty::Poly) -> Var {
        Var {
            id: self.id,
            source_name: self.source_name,
            bound: Some(bound),
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
    Wildcard(Option<ty::Poly>),
    List(Vec<Destruc>, Option<Box<Destruc>>),
}

#[derive(PartialEq, Debug)]
pub struct Fun {
    pvar_ids: Vec<ty::PVarId>,
    params: Destruc,
    ret_ty: Option<ty::Poly>,

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
}
