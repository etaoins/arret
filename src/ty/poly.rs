use ty::Ty;
use std::collections::HashSet;

pub struct PVar<'tcx> {
    source_name: &'tcx str,
    bound: &'tcx Ty<'tcx>,
}

pub enum PTy<'tcx> {
    Var(&'tcx Ty<'tcx>),
    Fixed(HashSet<&'tcx PDatum<'tcx>>, Option<&'tcx PFun<'tcx>>),
}

pub enum PDatum<'tcx> {
    Bool,
    Char,
    Int,
    SList(Vec<&'tcx PTy<'tcx>>),
    UList(&'tcx PTy<'tcx>),
    String,
    Symbol,
    SVec(Vec<&'tcx PTy<'tcx>>),
    UVec(&'tcx PTy<'tcx>),
}

pub struct PFun<'tcx> {
    pvars: Vec<&'tcx PVar<'tcx>>,

    impure: bool,
    args: Vec<&'tcx PTy<'tcx>>,
    ret: &'tcx PTy<'tcx>,
}
