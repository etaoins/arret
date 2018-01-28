use std::collections::BTreeSet;

#[derive(PartialEq, Debug, Hash)]
pub struct Ty<'tcx>(
    BTreeSet<&'tcx Datum<'tcx, Ty<'tcx>>>,
    Option<&'tcx Fun<'tcx, Ty<'tcx>>>,
);

#[derive(PartialEq, Debug, Hash)]
pub enum Datum<'tcx, S>
where
    S: 'tcx,
{
    Bool,
    Char,
    Int,
    SList(Vec<&'tcx S>),
    UList(&'tcx S),
    String,
    Symbol,
    SVec(Vec<&'tcx S>),
    UVec(&'tcx S),
}

#[derive(PartialEq, Debug, Hash)]
pub struct Fun<'tcx, S>
where
    S: 'tcx,
{
    impure: bool,
    args: Vec<&'tcx S>,
    ret: &'tcx S,
}

#[derive(Debug, Hash)]
pub struct PVar<'tcx> {
    inst_id: usize,
    source_name: &'tcx str,
    bound: &'tcx Ty<'tcx>,
}

impl<'tcx> PartialEq for PVar<'tcx> {
    fn eq(&self, other: &PVar<'tcx>) -> bool {
        self.inst_id == other.inst_id
    }
}

#[derive(PartialEq, Debug, Hash)]
pub enum PTy<'tcx> {
    Var(&'tcx PVar<'tcx>),
    Fixed(
        BTreeSet<&'tcx Datum<'tcx, PTy<'tcx>>>,
        Option<&'tcx Fun<'tcx, PTy<'tcx>>>,
    ),
}

pub type PFun<'tcx> = Fun<'tcx, PTy<'tcx>>;
