#[derive(PartialEq, Eq, Debug, Hash, Clone)]
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
    Fixed(Vec<NonFun<Poly>>, Option<Box<Fun<Poly>>>),
}

impl From<NonFun<Poly>> for Poly {
    fn from(non_fun: NonFun<Poly>) -> Poly {
        Poly::Fixed(vec![non_fun], None)
    }
}

impl From<Fun<Poly>> for Poly {
    fn from(fun: Fun<Poly>) -> Poly {
        Poly::Fixed(vec![], Some(Box::new(fun)))
    }
}

#[macro_export]
macro_rules! union_ty {
    ( $($t:expr),* ) => {{
        ty::Poly::Fixed(vec![$( $t, )*], None)
    }}
}
