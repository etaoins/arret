use std::collections::BTreeSet;

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub enum NonFun<S> {
    Bool(bool),
    List(Vec<S>, Option<S>),
    Vec(Option<S>, Vec<S>),
    Sym(String),
    AnySym,
    Int,
    Float,
    Str,
    Char,
    Set(S),
    Hash(S, S),
}

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub struct Fun<S> {
    impure: bool,
    params: S,
    ret: S,
}

impl<S> Fun<S> {
    pub fn new(impure: bool, params: S, ret: S) -> Fun<S> {
        Fun {
            impure,
            params,
            ret,
        }
    }
}

#[derive(Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub struct PVar {
    inst_id: usize,
    source_name: String,
    bound: Poly,
}

impl PartialEq for PVar {
    fn eq(&self, other: &PVar) -> bool {
        self.inst_id == other.inst_id
    }
}

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub enum Poly {
    //Var(PVar),
    Fixed(BTreeSet<NonFun<Poly>>, Option<Box<Fun<Poly>>>),
}

impl From<NonFun<Poly>> for Poly {
    fn from(non_fun: NonFun<Poly>) -> Poly {
        let mut non_fun_tys = BTreeSet::<NonFun<Poly>>::new();
        non_fun_tys.insert(non_fun.into());

        Poly::Fixed(non_fun_tys, None)
    }
}

impl From<Fun<Poly>> for Poly {
    fn from(fun: Fun<Poly>) -> Poly {
        Poly::Fixed(BTreeSet::new(), Some(Box::new(fun)))
    }
}

#[macro_export]
macro_rules! union_ty {
    ( $($t:expr),* ) => {{
        use std::collections::BTreeSet;
        let mut non_fun_tys = BTreeSet::<ty::NonFun<ty::Poly>>::new();

        $(
            non_fun_tys.insert($t.into());
        )*

        ty::Poly::Fixed(non_fun_tys, None)
    }}
}
