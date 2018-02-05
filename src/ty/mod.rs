use std::collections::BTreeSet;

#[derive(PartialEq, Debug, Hash)]
pub struct Ty(BTreeSet<Datum<Ty>>, Option<Box<Fun<Ty>>>);

#[derive(PartialEq, Debug, Hash)]
pub enum Datum<S> {
    Bool,
    Char,
    Int,
    Float,
    SList(Vec<S>),
    UList(S),
    String,
    Symbol,
    SVec(Vec<S>),
    UVec(S),
    Hash(S, S),
    Set(S),
}

#[derive(PartialEq, Debug, Hash)]
pub struct Fun<S> {
    impure: bool,
    fixed_args: Vec<S>,
    rest_arg: Option<S>,
    ret: S,
}

#[derive(Debug, Hash)]
pub struct PVar {
    inst_id: usize,
    source_name: String,
    bound: Ty,
}

impl PartialEq for PVar {
    fn eq(&self, other: &PVar) -> bool {
        self.inst_id == other.inst_id
    }
}

#[derive(PartialEq, Debug, Hash)]
pub enum PTy {
    Var(PVar),
    Fixed(BTreeSet<Datum<PTy>>, Option<Box<Fun<PTy>>>),
}

pub type PFun = Fun<PTy>;
