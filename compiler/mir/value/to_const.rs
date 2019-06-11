use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::Value;

pub fn list_to_const(
    ehx: &mut EvalHirCtx,
    fixed: &[Value],
    rest: Option<&Value>,
) -> Option<Gc<boxed::Any>> {
    let fixed_boxes = fixed
        .iter()
        .map(|value| value_to_const(ehx, value))
        .collect::<Option<Vec<Gc<boxed::Any>>>>()?;

    let rest_box = match rest {
        Some(rest) => {
            let rest_boxed = value_to_const(ehx, rest)?;
            if let Some(list_ref) = rest_boxed.downcast_ref::<boxed::List<boxed::Any>>() {
                list_ref
            } else {
                panic!("Attempted to build list with non-list tail");
            }
        }
        None => boxed::List::<boxed::Any>::empty(),
    };

    let list = boxed::List::<boxed::Any>::new_with_tail(ehx, fixed_boxes.into_iter(), rest_box);

    Some(list.as_any_ref())
}

/// Attempts to convert a MIR value to a constant boxed values
///
/// Regs do not have a constant value at compile type; they will return None
pub fn value_to_const(ehx: &mut EvalHirCtx, value: &Value) -> Option<Gc<boxed::Any>> {
    match value {
        Value::Const(boxed) => Some(*boxed),
        Value::List(fixed, Some(rest)) => list_to_const(ehx, fixed, Some(&*rest)),
        Value::List(fixed, None) => list_to_const(ehx, fixed, None),
        Value::Record(record_cons, fields) => {
            if !fields.is_empty() {
                unimplemented!("boxing records with fields");
            }

            let evaled_record_class = ehx.evaled_record_class_for_cons(record_cons);
            let jit_record_class_id = evaled_record_class.jit_record_class_id;

            Some(boxed::Record::new(ehx, jit_record_class_id, &[]).as_any_ref())
        }
        Value::TyPred(test_ty) => {
            let ty_pred_arret_fun = ehx
                .synthetic_funs()
                .ty_pred_arret_fun(test_ty.clone())
                .clone();

            ehx.arret_fun_to_jit_boxed(&ty_pred_arret_fun)
                .map(|f| f.as_any_ref())
        }
        Value::EqPred => {
            let eq_pred_arret_fun = ehx.synthetic_funs().eq_pred_arret_fun().clone();
            ehx.arret_fun_to_jit_boxed(&eq_pred_arret_fun)
                .map(|f| f.as_any_ref())
        }
        Value::ArretFun(ref arret_fun) => ehx
            .arret_fun_to_jit_boxed(arret_fun)
            .map(|f| f.as_any_ref()),
        Value::RecordCons(_) => {
            unimplemented!("converting record constructors to constants");
        }
        Value::FieldAccessor(_, _) => {
            unimplemented!("converting record field accessors to constants");
        }
        Value::RustFun(ref rust_fun) => {
            Some(ehx.rust_fun_to_jit_boxed(rust_fun.clone()).as_any_ref())
        }
        Value::Reg(_) => None,
    }
}
