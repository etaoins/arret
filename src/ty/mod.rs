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

#[derive(Eq, Debug, Hash, Clone)]
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

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Poly {
    //Var(PVar),
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
