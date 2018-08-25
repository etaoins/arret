use std::collections::HashMap;
use std::rc::Rc;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::intern::Interner;

use crate::hir;
use crate::ty;

#[derive(Clone)]
pub struct Closure {
    pub captures: HashMap<hir::VarId, Value>,
    pub fun_expr: Rc<hir::Fun<ty::Poly>>,
}

#[derive(Clone)]
pub enum Value {
    Const(Gc<boxed::Any>),
    // This uses Box<[]> because we can't convert from a Vec<> to Rc<[]> without reallocating
    List(Box<[Value]>, Option<Box<Value>>),
    Closure(Closure),
    RustFun(Rc<hir::rfi::Fun>),
    TyPred(Rc<ty::Poly>),
}

impl Value {
    pub fn list_iter(&self) -> ListIterator<'_> {
        match self {
            Value::List(fixed, rest) => ListIterator {
                fixed,
                rest: rest.as_ref().map(|rest| rest.as_ref()),
            },
            other => ListIterator {
                fixed: &[],
                rest: Some(other),
            },
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
    }
}

pub fn poly_for_value(interner: &Interner, value: &Value) -> ty::Poly {
    match value {
        Value::Const(any_ref) => type_for_any_ref(interner, *any_ref),
        Value::List(fixed_values, rest_value) => {
            let fixed_polys = fixed_values
                .iter()
                .map(|value| poly_for_value(interner, value))
                .collect::<Vec<ty::Poly>>();

            let rest_poly = rest_value
                .as_ref()
                .map(|value| poly_for_value(interner, value.as_ref()));

            ty::Ty::List(ty::List::new(fixed_polys.into_boxed_slice(), rest_poly)).into_poly()
        }
        Value::RustFun(rust_fun) => {
            ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into_poly()
        }
        Value::TyPred(_) => ty::Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into_poly(),
        Value::Closure(closure) => {
            use crate::hir::destruc::poly_for_list_destruc;
            let fun_expr = &closure.fun_expr;

            let top_fun = ty::TopFun::new(fun_expr.purity.clone(), fun_expr.ret_ty.clone());
            let params_poly = poly_for_list_destruc(&fun_expr.params);

            let fun = ty::Fun::new(
                fun_expr.pvar_ids.clone(),
                fun_expr.tvar_ids.clone(),
                top_fun,
                params_poly,
            );

            ty::Ty::Fun(Box::new(fun)).into_poly()
        }
    }
}

pub struct ListIterator<'list> {
    fixed: &'list [Value],
    rest: Option<&'list Value>,
}

impl<'list> ListIterator<'list> {
    pub fn next_unchecked(&mut self) -> &'list Value {
        if self.fixed.is_empty() {
            if let Some(Value::List(fixed, rest)) = self.rest {
                // Become our tail
                self.fixed = fixed;
                self.rest = rest.as_ref().map(|rest| rest.as_ref());

                self.next_unchecked()
            } else {
                unimplemented!("Need to pull element off rest")
            }
        } else {
            let next = self.fixed.first().unwrap();
            self.fixed = &self.fixed[1..];
            next
        }
    }

    pub fn rest(&self) -> Value {
        Value::List(
            self.fixed.to_vec().into_boxed_slice(),
            self.rest.cloned().map(Box::new),
        )
    }
}

pub fn visit_value_root(collection: &mut boxed::Collection, value: &mut Value) {
    match value {
        Value::Const(ref mut any_ref) => collection.visit_box(any_ref),
        Value::List(ref mut fixed, ref mut rest) => {
            for any_ref in fixed.iter_mut() {
                visit_value_root(collection, any_ref);
            }
            for any_ref in rest {
                visit_value_root(collection, any_ref);
            }
        }
        Value::Closure(ref mut closure) => {
            for any_ref in closure.captures.values_mut() {
                visit_value_root(collection, any_ref);
            }
        }
        _ => {}
    }
}
