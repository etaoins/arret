pub mod build_reg;
pub mod list;
pub mod to_const;

use std::collections::HashMap;
use std::rc::Rc;

use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::intern::Interner;

use crate::hir;
use crate::mir::ops::RegId;
use crate::ty;
use crate::ty::purity::Purity;

#[derive(Clone, Debug)]
pub struct ArretFun {
    pub span: Span,
    pub source_name: Option<String>,
    pub captures: HashMap<hir::VarId, Value>,
    pub fun_expr: Rc<hir::Fun<hir::Inferred>>,
}

#[derive(Clone, Debug)]
pub struct RegValue {
    pub reg: RegId,
    pub abi_type: abitype::ABIType,
}

#[derive(Clone, Debug)]
pub enum Value {
    Const(Gc<boxed::Any>),
    // This uses Box<[]> because we can't convert from a Vec<> to Rc<[]> without reallocating
    List(Box<[Value]>, Option<Box<Value>>),
    ArretFun(ArretFun),
    RustFun(Rc<hir::rfi::Fun>),
    TyPred(Rc<ty::Mono>),
    Reg(Rc<RegValue>),
    Divergent,
}

impl Value {
    pub fn list_iter(&self) -> list::ListIterator {
        use std::ops::Deref;
        match self {
            Value::List(fixed, Some(rest)) => {
                list::ListIterator::new(fixed.clone().into_vec(), Some(rest.deref().clone()))
            }
            Value::List(fixed, None) => list::ListIterator::new(fixed.clone().into_vec(), None),
            other => list::ListIterator::new(vec![], Some(other.clone())),
        }
    }

    pub fn into_list_iter(self) -> list::ListIterator {
        match self {
            Value::List(fixed, Some(rest)) => {
                list::ListIterator::new(fixed.into_vec(), Some(*rest))
            }
            Value::List(fixed, None) => list::ListIterator::new(fixed.into_vec(), None),
            other => list::ListIterator::new(vec![], Some(other)),
        }
    }

    pub fn is_divergent(&self) -> bool {
        match self {
            Value::Divergent => true,
            _ => false,
        }
    }
}

fn type_for_any_ref<S: ty::TyRef>(interner: &Interner, any_ref: Gc<boxed::Any>) -> S {
    use runtime::boxed::AnySubtype;

    match any_ref.as_subtype() {
        AnySubtype::True(_) => ty::Ty::LitBool(true).into_ty_ref(),
        AnySubtype::False(_) => ty::Ty::LitBool(false).into_ty_ref(),
        AnySubtype::Nil(_) => ty::Ty::List(ty::List::empty()).into_ty_ref(),
        AnySubtype::Str(_) => ty::Ty::Str.into_ty_ref(),
        AnySubtype::Int(_) => ty::Ty::Int.into_ty_ref(),
        AnySubtype::Float(_) => ty::Ty::Float.into_ty_ref(),
        AnySubtype::Char(_) => ty::Ty::Char.into_ty_ref(),
        AnySubtype::Sym(sym_ref) => ty::Ty::LitSym(sym_ref.name(interner).into()).into_ty_ref(),
        AnySubtype::TopVector(top_vector_ref) => {
            let vector_ref = top_vector_ref.as_vector();
            let elem_types = vector_ref
                .iter()
                .map(|elem| type_for_any_ref(interner, *elem))
                .collect::<Vec<S>>();

            ty::Ty::Vector(elem_types.into_boxed_slice()).into_ty_ref()
        }
        AnySubtype::TopPair(top_pair) => {
            let list_ref = top_pair.as_pair().as_list();
            let elem_types = list_ref
                .iter()
                .map(|elem| type_for_any_ref(interner, elem))
                .collect::<Vec<S>>();

            ty::Ty::List(ty::List::new(elem_types.into_boxed_slice(), None)).into_ty_ref()
        }
        AnySubtype::FunThunk(_) => {
            ty::TopFun::new(Purity::Impure.into_poly(), ty::Ty::Any.into_poly()).into_ty_ref()
        }
    }
}

pub fn mono_for_value(interner: &Interner, value: &Value) -> ty::Mono {
    match value {
        Value::Const(any_ref) => type_for_any_ref(interner, *any_ref),
        Value::List(fixed_values, rest_value) => {
            let fixed_polys = fixed_values
                .iter()
                .map(|value| mono_for_value(interner, value))
                .collect::<Vec<ty::Mono>>();

            let rest_poly = rest_value
                .as_ref()
                .map(|value| mono_for_value(interner, value.as_ref()));

            ty::Ty::List(ty::List::new(fixed_polys.into_boxed_slice(), rest_poly)).into_mono()
        }
        Value::RustFun(rust_fun) => {
            ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into_mono()
        }
        Value::TyPred(_) => ty::Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into_mono(),
        Value::ArretFun(arret_fun) => {
            use crate::hir::destruc::poly_for_list_destruc;
            let fun_expr = &arret_fun.fun_expr;

            let top_fun = ty::TopFun::new(fun_expr.purity.clone(), fun_expr.ret_ty.clone());
            let params_poly = poly_for_list_destruc(&fun_expr.params);

            let fun = ty::Fun::new(
                fun_expr.pvars.clone(),
                fun_expr.tvars.clone(),
                top_fun,
                params_poly,
            );

            ty::Ty::Fun(Box::new(fun)).into_mono()
        }
        Value::Reg(reg_value) => {
            use crate::ty::conv_abi::ConvertableABIType;
            reg_value.abi_type.to_ty_ref()
        }
        Value::Divergent => ty::Ty::never().into_mono(),
    }
}

pub fn visit_value_root(strong_pass: &mut boxed::collect::StrongPass, value: &mut Value) {
    match value {
        Value::Const(ref mut any_ref) => strong_pass.visit_box(any_ref),
        Value::List(ref mut fixed, ref mut rest) => {
            for any_ref in fixed.iter_mut() {
                visit_value_root(strong_pass, any_ref);
            }
            for any_ref in rest {
                visit_value_root(strong_pass, any_ref);
            }
        }
        Value::ArretFun(ref mut arret_fun) => {
            for any_ref in arret_fun.captures.values_mut() {
                visit_value_root(strong_pass, any_ref);
            }
        }
        _ => {}
    }
}
