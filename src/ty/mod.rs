use std::collections::BTreeSet;

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub struct Ty(BTreeSet<NonFun<Ty>>, Option<Box<Fun<Ty>>>);

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub enum NonFun<S> {
    Bool(bool),
    List(Vec<S>, Option<S>),
    Sym(String),
    /*
    Char,
    Int,
    Float,
    Str,
    AnySym,
    Vec(Vec<S>, Option<S>),
    Hash(S, S),
    Set(S),
    */
}

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub struct Fun<S> {
    impure: bool,
    fixed_args: Vec<S>,
    rest_arg: Option<S>,
    ret: S,
}

#[derive(Eq, Debug, Hash, PartialOrd, Ord, Clone)]
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

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
pub enum PTy {
    //Var(PVar),
    Fixed(BTreeSet<NonFun<PTy>>, Option<Box<Fun<PTy>>>),
}

impl From<NonFun<PTy>> for PTy {
    fn from(non_fun: NonFun<PTy>) -> PTy {
        let mut non_fun_tys = BTreeSet::<NonFun<PTy>>::new();
        non_fun_tys.insert(non_fun.into());

        PTy::Fixed(non_fun_tys, None)
    }
}

#[macro_export]
macro_rules! union {
    ( $($t:expr),* ) => {{
        use std::collections::BTreeSet;
        let mut non_fun_tys = BTreeSet::<ty::NonFun<ty::PTy>>::new();

        $(
            non_fun_tys.insert($t.into());
        )*

        ty::PTy::Fixed(non_fun_tys, None)
    }}
}

//pub type PFun = Fun<PTy>;
