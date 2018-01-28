use ty::poly::PFun;
use ty::Ty;
use syntax::span::Span;
use syntax::value::Value;

pub struct Var<'tcx> {
    source_name: &'tcx str,
    bound: &'tcx Ty<'tcx>,
}

pub struct Fun<'tcx> {
    source_name: &'tcx str,
    sig: &'tcx PFun<'tcx>,
    args: Vec<&'tcx Var<'tcx>>,
}

pub struct Cond<'tcx> {
    test: &'tcx Expr<'tcx>,
    true_expr: &'tcx Expr<'tcx>,
    false_expr: &'tcx Expr<'tcx>,
}

pub enum Expr<'tcx> {
    Lit(Span, Value),
    App(Span, &'tcx Expr<'tcx>, Vec<&'tcx Expr<'tcx>>),
    Fun(Span, Fun<'tcx>),
    Def(Span, &'tcx Var<'tcx>, &'tcx Expr<'tcx>),
    Cond(Span, Cond<'tcx>),
    Ref(Span, &'tcx Var<'tcx>),
}
