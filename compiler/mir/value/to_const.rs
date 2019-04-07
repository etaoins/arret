use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

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
    use crate::mir::value::synthetic_fun::{eq_pred_arret_fun, ty_pred_arret_fun};

    match value {
        Value::Const(boxed) => Some(*boxed),
        Value::List(fixed, Some(rest)) => list_to_const(ehx, fixed, Some(&*rest)),
        Value::List(fixed, None) => list_to_const(ehx, fixed, None),
        Value::TyPred(test_ty) => ehx
            .arret_fun_to_jit_boxed(&ty_pred_arret_fun(*test_ty))
            .map(|f| f.as_any_ref()),

        Value::EqPred => ehx
            .arret_fun_to_jit_boxed(&eq_pred_arret_fun())
            .map(|f| f.as_any_ref()),

        Value::ArretFun(ref arret_fun) => ehx
            .arret_fun_to_jit_boxed(arret_fun)
            .map(|f| f.as_any_ref()),

        Value::RustFun(ref rust_fun) => {
            Some(ehx.rust_fun_to_jit_boxed(rust_fun.clone()).as_any_ref())
        }
        Value::Reg(_) => None,
    }
}
