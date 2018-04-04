#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty<S> {
    AnySym,
    Bool(bool),
    Char,
    Float,
    Fun(Box<Fun<S>>),
    Hash(Box<S>, Box<S>),
    Int,
    List(Vec<S>, Option<Box<S>>),
    Set(Box<S>),
    Str,
    Sym(String),
    Union(Vec<S>),
    Vec(Option<Box<S>>, Vec<S>),
}

impl<S> Ty<S> {
    pub fn new_fun(impure: bool, params: S, ret: S) -> Ty<S> {
        Ty::Fun(Box::new(Fun::new(impure, params, ret)))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PVarId(usize);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: String,
    bound: Option<Poly>,
}

impl PVar {
    pub fn new(source_name: String, bound: Option<Poly>) -> PVar {
        PVar { source_name, bound }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Poly {
    Var(PVarId),
    Fixed(Ty<Poly>),
}

impl From<Ty<Poly>> for Poly {
    fn from(ty: Ty<Poly>) -> Poly {
        Poly::Fixed(ty)
    }
}
