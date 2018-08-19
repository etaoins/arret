use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::hir;
use crate::ty;

#[derive(Clone)]
pub enum Value {
    Const(Gc<boxed::Any>),
    Fun(Box<hir::Fun<ty::Poly>>),
    RustFun(Box<hir::rfi::Fun>),
    TyPred(ty::Poly),
}
