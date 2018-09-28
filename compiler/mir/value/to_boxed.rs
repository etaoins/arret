use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::Value;

pub fn list_to_boxed(
    ehx: &mut EvalHirCtx,
    fixed: &[Value],
    rest: Option<&Value>,
) -> Option<Gc<boxed::Any>> {
    let fixed_boxes = fixed
        .iter()
        .map(|value| value_to_boxed(ehx, value))
        .collect::<Option<Vec<Gc<boxed::Any>>>>()?;

    let rest_box = match rest {
        Some(rest) => {
            let rest_boxed = value_to_boxed(ehx, rest)?;
            if let Some(top_list) = rest_boxed.downcast_ref::<boxed::TopList>() {
                top_list.as_list()
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
pub fn value_to_boxed(ehx: &mut EvalHirCtx, value: &Value) -> Option<Gc<boxed::Any>> {
    match value {
        Value::Const(boxed) => Some(*boxed),
        Value::List(fixed, Some(rest)) => list_to_boxed(ehx, fixed, Some(&*rest)),
        Value::List(fixed, None) => list_to_boxed(ehx, fixed, None),
        Value::TyPred(ref test_poly) => {
            unimplemented!("Boxing of type predicates: {:?}", test_poly)
        }
        Value::Closure(ref closure) => {
            unimplemented!("Boxing of Arret closures: {:?}", closure.fun_expr)
        }
        Value::RustFun(ref rust_fun) => Some(ehx.rust_fun_to_boxed(value, rust_fun).as_any_ref()),
        Value::Reg(_) => None,
        Value::Divergent => None,
    }
}
