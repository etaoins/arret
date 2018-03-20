pub mod lowering;
mod scope;
mod error;
mod module;
mod loader;
mod macros;

use syntax::span::Span;
use syntax::value::Value;
use ty;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct VarId(usize);

impl VarId {
    fn new(id: usize) -> VarId {
        VarId(id)
    }
}

#[derive(Debug)]
pub struct Var<T> {
    id: VarId,
    source_name: String,
    bound: Option<T>,
}

impl<T> PartialEq for Var<T> {
    fn eq(&self, other: &Var<T>) -> bool {
        self.id.0 == other.id.0
    }
}

#[derive(Debug, PartialEq)]
pub enum Destruc<T> {
    Var(Var<T>),
    List(Vec<Destruc<T>>, Option<Box<Destruc<T>>>),
}

#[derive(PartialEq, Debug)]
pub struct Fun {
    source_name: Option<String>,

    poly_vars: Vec<ty::PVar>,
    params: Destruc<ty::PTy>,
    ret_ty: Option<ty::PTy>,

    body_expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Cond {
    test_expr: Box<Expr>,
    true_expr: Box<Expr>,
    false_expr: Box<Expr>,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Lit(Value),
    App(Span, Box<Expr>, Vec<Expr>),
    Fun(Span, Fun),
    Def(Span, Destruc<ty::Ty>, Box<Expr>),
    Cond(Span, Cond),
    Ref(Span, VarId),
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
