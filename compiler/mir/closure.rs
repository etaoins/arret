use std::collections::HashMap;
use std::rc::Rc;

use syntax::span::Span;

use crate::hir;
use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::Value;

type ValueVec = Vec<(hir::VarId, Value)>;

/// Tracks the constant and free values captured by an expression
#[derive(Clone, Debug)]
pub struct Closure {
    pub const_values: ValueVec,
    pub free_values: ValueVec,
}

impl Closure {
    pub fn empty() -> Closure {
        Closure {
            const_values: vec![],
            free_values: vec![],
        }
    }
}

fn can_reference_local_regs(value: &Value) -> bool {
    match value {
        Value::Const(_)
        | Value::EqPred
        | Value::TyPred(_)
        | Value::RustFun(_)
        // TODO: Is this correct? My intuition is that the `ArretFun` would have to come from a
        // nested expression so we would already have accounted for any values it closes over.
        | Value::ArretFun(_) => false,
        _ => true,
    }
}

/// Calculates the values captured by the passed expression
pub fn calculate_closure(
    local_values: &HashMap<hir::VarId, Value>,
    capturing_expr: &hir::Expr<hir::Inferred>,
) -> Closure {
    let mut captured_values = HashMap::new();

    // Only process captures if there are local values. This is to avoid visiting the expression
    // when capturing isn't possible
    if !local_values.is_empty() {
        // Look for references to variables inside the function
        hir::visitor::visit_exprs(&capturing_expr, &mut |expr| {
            if let hir::ExprKind::Ref(var_id) = &expr.kind {
                // Avoiding cloning the value if we've already captured
                if !captured_values.contains_key(var_id) {
                    if let Some(value) = local_values.get(var_id) {
                        // Local value is referenced; capture
                        captured_values.insert(*var_id, value.clone());
                    }
                }
            }
        });
    }

    // Determine which captures are constants
    type ValueVec = Vec<(hir::VarId, Value)>;
    let (free_values, const_values): (ValueVec, ValueVec) = captured_values
        .into_iter()
        .partition(|(_, value)| can_reference_local_regs(value));

    Closure {
        const_values,
        free_values,
    }
}

/// Builds code to save local values into a closure
pub fn save_to_closure_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    closure: &Closure,
) -> Option<BuiltReg> {
    match closure.free_values.first() {
        Some((_, value)) => {
            use crate::mir::value::build_reg::value_to_reg;
            use runtime::abitype;

            Some(value_to_reg(
                ehx,
                b,
                span,
                value,
                &abitype::BoxedABIType::Any.into(),
            ))
        }
        None => None,
    }
}

/// Loads a closure assuming all captured variables are still inside the local function
pub fn load_from_current_fun(local_values: &mut HashMap<hir::VarId, Value>, closure: &Closure) {
    local_values.extend(
        closure
            .const_values
            .iter()
            .chain(closure.free_values.iter())
            .map(|(var_id, value)| (*var_id, value.clone())),
    );
}

/// Loads a closure from a closure parameter
pub fn load_from_closure_param(
    local_values: &mut HashMap<hir::VarId, Value>,
    closure: &Closure,
    closure_reg: Option<BuiltReg>,
) {
    use crate::mir::value;
    use runtime::abitype;

    if closure.free_values.len() > 1 {
        // We can either place these in a list or wait for record support
        unimplemented!("capturing multiple free values");
    }

    // Include the const values directly
    local_values.extend(
        closure
            .const_values
            .iter()
            .map(|(var_id, value)| (*var_id, value.clone())),
    );

    if let Some((var_id, _)) = closure.free_values.first() {
        let closure_reg = closure_reg.unwrap();

        local_values.insert(
            *var_id,
            Value::Reg(Rc::new(value::RegValue::new(
                closure_reg,
                abitype::BoxedABIType::Any.into(),
            ))),
        );
    }
}
