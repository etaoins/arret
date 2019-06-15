use std::collections::HashMap;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::hir;
use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::{RecordStruct, RecordStructId};
use crate::mir::specific_abi_type::specific_abi_type_for_value;
use crate::mir::value::Value;

type Values = Box<[(hir::VarId, Value)]>;

#[derive(Clone, Debug)]
enum Repr {
    Empty,
    SingleBox,
    RecordStruct(RecordStructId),
}

/// Tracks the constant and free values captured by an expression
#[derive(Clone, Debug)]
pub struct Closure {
    pub const_values: Values,
    pub free_values: Values,
    repr: Repr,
}

impl Closure {
    pub fn empty() -> Closure {
        Closure {
            const_values: Box::new([]),
            free_values: Box::new([]),
            repr: Repr::Empty,
        }
    }
}

fn can_reference_local_regs(value: &Value) -> bool {
    match value {
        Value::Const(_)
        | Value::EqPred
        | Value::TyPred(_)
        | Value::RecordCons(_)
        | Value::FieldAccessor(_, _)
        | Value::RustFun(_) => false,
        Value::Reg(_) => true,
        Value::ArretFun(arret_fun) => !arret_fun.closure().free_values.is_empty(),
        Value::List(fixed, rest) => {
            let mut inner_values = fixed.iter().chain(rest.iter().map(AsRef::as_ref));
            !inner_values.any(can_reference_local_regs)
        }
        Value::Record(_, fields) => fields.iter().any(can_reference_local_regs),
    }
}

/// Calculates the values captured by the passed expression
pub fn calculate_closure(
    local_values: &HashMap<hir::VarId, Value>,
    capturing_expr: &hir::Expr<hir::Inferred>,
    source_name: Option<&DataStr>,
) -> Closure {
    let mut captured_values = HashMap::new();

    // Only process captures if there are local values. This is to avoid visiting the expression
    // when capturing isn't possible
    if !local_values.is_empty() {
        // Look for references to variables inside the function
        hir::visitor::visit_exprs(&capturing_expr, &mut |expr| {
            if let hir::ExprKind::Ref(_, var_id) = &expr.kind {
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
    let (free_values, const_values): (Vec<_>, Vec<_>) = captured_values
        .into_iter()
        .partition(|(_, value)| can_reference_local_regs(value));

    let repr = match free_values.len() {
        0 => Repr::Empty,
        1 => {
            // Single field records can never box as efficiently as our native box representation
            Repr::SingleBox
        }
        _ => {
            let closure_source_name = source_name
                .map(|source_name| format!("{}_closure", source_name).into())
                .unwrap_or("anon_closure".into());

            let field_abi_types = free_values
                .iter()
                .map(|(_, value)| specific_abi_type_for_value(value))
                .collect();

            let record_struct_id = RecordStruct::new(closure_source_name, field_abi_types);
            Repr::RecordStruct(record_struct_id)
        }
    };

    Closure {
        const_values: const_values.into_boxed_slice(),
        free_values: free_values.into_boxed_slice(),
        repr,
    }
}

/// Builds code to save local values into a closure
pub fn save_to_closure_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    closure: &Closure,
) -> Option<BuiltReg> {
    use crate::mir::value::build_reg::value_to_reg;
    use arret_runtime::abitype;

    match &closure.repr {
        Repr::Empty => None,
        Repr::SingleBox => {
            let value = &closure.free_values[0].1;

            Some(value_to_reg(
                ehx,
                b,
                span,
                value,
                &abitype::BoxedABIType::Any.into(),
            ))
        }
        Repr::RecordStruct(record_struct) => {
            use crate::mir::ops::*;

            let field_regs = closure
                .free_values
                .iter()
                .zip(record_struct.field_abi_types.iter())
                .map(|((_, value), abi_type)| value_to_reg(ehx, b, span, value, abi_type).into())
                .collect();

            let record_reg = b.push_reg(
                span,
                OpKind::AllocBoxedRecord,
                BoxRecordOp {
                    record_struct: record_struct.clone(),
                    field_regs,
                },
            );

            Some(b.cast_boxed(span, record_reg, abitype::BoxedABIType::Any))
        }
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
    b: &mut Builder,
    span: Span,
    local_values: &mut HashMap<hir::VarId, Value>,
    closure: &Closure,
    closure_reg: Option<BuiltReg>,
) {
    use crate::mir::value;
    use arret_runtime::abitype;

    // Include the const values directly
    local_values.extend(
        closure
            .const_values
            .iter()
            .map(|(var_id, value)| (*var_id, value.clone())),
    );

    match &closure.repr {
        Repr::Empty => {}
        Repr::SingleBox => {
            let var_id = &closure.free_values[0].0;
            let closure_reg = closure_reg.unwrap();

            local_values.insert(
                *var_id,
                value::RegValue::new(closure_reg, abitype::BoxedABIType::Any.into()).into(),
            );
        }
        Repr::RecordStruct(record_struct) => {
            use crate::mir::ops::*;

            let record_reg: RegId = closure_reg.unwrap().into();

            for (field_index, (var_id, _)) in closure.free_values.iter().enumerate() {
                let field_reg = b.push_reg(
                    span,
                    OpKind::LoadBoxedRecordField,
                    LoadBoxedRecordFieldOp {
                        record_reg,
                        record_struct: record_struct.clone(),
                        field_index,
                    },
                );

                let field_abi_type = record_struct.field_abi_types[field_index].clone();

                local_values.insert(
                    *var_id,
                    value::RegValue::new(field_reg, field_abi_type).into(),
                );
            }
        }
    }
}
