mod lowering;

use syntax::span::Span;
use syntax::value::Value;
use ty;

#[derive(Debug)]
pub struct Var<'tcx> {
    inst_id: usize,
    source_name: &'tcx str,
    bound: &'tcx ty::Ty<'tcx>,
}

impl<'tcx> PartialEq for Var<'tcx> {
    fn eq(&self, other: &Var<'tcx>) -> bool {
        self.inst_id == other.inst_id
    }
}

#[derive(PartialEq, Debug)]
pub struct Fun<'tcx> {
    source_name: &'tcx str,
    ty: ty::PFun<'tcx>,
    args: Vec<&'tcx Var<'tcx>>,
}

#[derive(PartialEq, Debug)]
pub struct Cond<'tcx> {
    test: &'tcx Expr<'tcx>,
    true_expr: &'tcx Expr<'tcx>,
    false_expr: &'tcx Expr<'tcx>,
}

#[derive(PartialEq, Debug)]
pub enum Expr<'tcx> {
    Lit(Value),
    App(Span, &'tcx Expr<'tcx>, Vec<&'tcx Expr<'tcx>>),
    Fun(Span, Fun<'tcx>),
    Def(Span, &'tcx Var<'tcx>, &'tcx Expr<'tcx>),
    Cond(Span, Cond<'tcx>),
    Ref(Span, &'tcx Var<'tcx>),
    Do(Span, Vec<&'tcx Expr<'tcx>>),
}
