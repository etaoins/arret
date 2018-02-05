pub mod lowering;
mod scope;
mod error;

use syntax::span::{Span, EMPTY_SPAN};
use syntax::value::Value;
use ty;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    bound: Option<ty::Ty>,
}

impl PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        self.id.0 == other.id.0
    }
}

#[derive(PartialEq, Debug)]
pub struct Fun {
    source_name: String,
    ty: Option<ty::PFun>,
    args: Vec<Var>,
}

#[derive(PartialEq, Debug)]
pub struct Cond {
    test: Box<Expr>,
    true_expr: Box<Expr>,
    false_expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Lit(Value),
    App(Span, Box<Expr>, Vec<Expr>),
    Fun(Span, Fun),
    Def(Span, Var, Box<Expr>),
    Cond(Span, Cond),
    Ref(Span, VarId),
    Do(Span, Vec<Expr>),
}

impl Expr {
    fn from_vec(mut exprs: Vec<Expr>) -> Expr {
        if exprs.len() == 1 {
            exprs.pop().unwrap()
        } else {
            Expr::Do(EMPTY_SPAN, exprs)
        }
    }
}
