pub mod poly;

use std::collections::HashSet;

pub struct Ty<'tcx>(HashSet<&'tcx Datum<'tcx>>, Option<&'tcx Fun<'tcx>>);

pub enum Datum<'tcx> {
    Bool,
    Char,
    Int,
    SList(Vec<&'tcx Ty<'tcx>>),
    UList(&'tcx Ty<'tcx>),
    String,
    Symbol,
    SVec(Vec<&'tcx Ty<'tcx>>),
    UVec(&'tcx Ty<'tcx>),
}

pub struct Fun<'tcx> {
    impure: bool,
    args: Vec<&'tcx Ty<'tcx>>,
    ret: &'tcx Ty<'tcx>,
}
