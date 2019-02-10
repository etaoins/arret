use crate::ty;
use crate::ty::purity::Purity;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum TestTy {
    Sym,
    Str,
    Bool,
    Num,
    Int,
    Float,
    Char,
    List,
    Vector,
    Set,
    Map,
    Fun,
    Nil,
}

impl TestTy {
    pub fn to_ty<S: ty::TyRef>(self) -> ty::Ty<S> {
        match self {
            TestTy::Sym => ty::Ty::Sym,
            TestTy::Str => ty::Ty::Str,
            TestTy::Bool => ty::Ty::Bool,
            TestTy::Num => ty::Ty::Num,
            TestTy::Int => ty::Ty::Int,
            TestTy::Float => ty::Ty::Float,
            TestTy::Char => ty::Ty::Char,
            TestTy::List => {
                ty::Ty::List(ty::List::new(Box::new([]), Some(ty::Ty::Any.into_ty_ref())))
            }
            TestTy::Vector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into_ty_ref())),
            TestTy::Set => ty::Ty::Set(Box::new(ty::Ty::Any.into_ty_ref())),
            TestTy::Map => ty::Ty::Map(Box::new(ty::Map::new(
                ty::Ty::Any.into_ty_ref(),
                ty::Ty::Any.into_ty_ref(),
            ))),
            TestTy::Fun => {
                ty::TopFun::new(Purity::Impure.into_poly(), ty::Ty::Any.into_poly()).into_ty()
            }
            TestTy::Nil => ty::Ty::List(ty::List::empty()),
        }
    }

    pub fn to_str(self) -> &'static str {
        match self {
            TestTy::Str => "str?",
            TestTy::Sym => "sym?",
            TestTy::Num => "num?",
            TestTy::Int => "int?",
            TestTy::Float => "float?",
            TestTy::Bool => "bool?",
            TestTy::Char => "char?",
            TestTy::List => "list?",
            TestTy::Vector => "vector?",
            TestTy::Set => "set?",
            TestTy::Map => "map?",
            TestTy::Fun => "fn?",
            TestTy::Nil => "nil?",
        }
    }
}
