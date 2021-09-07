use std::collections::HashMap;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::hir;
use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::{RecordStruct, RecordStructId};
use crate::mir::specific_abi_type::specific_abi_type_for_value;
use crate::mir::value::Value;

type Values = Box<[(hir::LocalId, Value)]>;

/// Indicates the layout of captured values
#[derive(Clone, Debug)]
enum CapturesRepr {
    Empty,
    SingleBox,
    RecordStruct(RecordStructId),
}

/// Tracks the constant and free values captured by an expression
#[derive(Clone, Debug)]
pub struct EnvValues {
    pub const_values: Values,
    pub free_values: Values,
    captures_repr: CapturesRepr,
}

impl EnvValues {
    pub fn empty() -> EnvValues {
        EnvValues {
            const_values: Box::new([]),
            free_values: Box::new([]),
            captures_repr: CapturesRepr::Empty,
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
        Value::ArretFun(arret_fun) => !arret_fun.env_values().free_values.is_empty(),
        Value::List(fixed, rest) => fixed
            .iter()
            .chain(rest.iter().map(AsRef::as_ref))
            .any(can_reference_local_regs),
        Value::Record(_, fields) => fields.iter().any(can_reference_local_regs),
    }
}

/// Calculates the values captured from the environment by the passed expression
pub fn calculate_env_values(
    local_values: &HashMap<hir::LocalId, Value>,
    capturing_expr: &hir::Expr<hir::Inferred>,
    source_name: Option<&DataStr>,
) -> EnvValues {
    let mut captured_values = HashMap::new();

    // Only process captures if there are local values. This is to avoid visiting the expression
    // when capturing isn't possible
    if !local_values.is_empty() {
        // Look for references to variables inside the function
        hir::visitor::visit_exprs(capturing_expr, &mut |expr| {
            if let hir::ExprKind::LocalRef(_, local_id) = &expr.kind {
                if !captured_values.contains_key(local_id) {
                    if let Some(value) = local_values.get(local_id) {
                        captured_values.insert(*local_id, value.clone());
                    }
                }
            }
        });
    }

    // Determine which captures are constants
    let (free_values, const_values): (Vec<_>, Vec<_>) = captured_values
        .into_iter()
        .partition(|(_, value)| can_reference_local_regs(value));

    let captures_repr = match free_values.len() {
        0 => CapturesRepr::Empty,
        1 => {
            // Single field records can never box as efficiently as our native box representation
            CapturesRepr::SingleBox
        }
        _ => {
            let captures_source_name = source_name
                .map(|source_name| format!("{}_captures", source_name).into())
                .unwrap_or_else(|| "anon_captures".into());

            let field_abi_types = free_values
                .iter()
                .map(|(_, value)| specific_abi_type_for_value(value))
                .collect();

            let record_struct_id = RecordStruct::new(captures_source_name, field_abi_types);
            CapturesRepr::RecordStruct(record_struct_id)
        }
    };

    EnvValues {
        const_values: const_values.into_boxed_slice(),
        free_values: free_values.into_boxed_slice(),
        captures_repr,
    }
}

/// Builds code to save local values into a captures reg
pub fn save_to_captures_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    env_values: &EnvValues,
) -> Option<BuiltReg> {
    use crate::mir::value::build_reg::value_to_reg;
    use arret_runtime::abitype;

    match &env_values.captures_repr {
        CapturesRepr::Empty => None,
        CapturesRepr::SingleBox => {
            let value = &env_values.free_values[0].1;

            Some(value_to_reg(
                ehx,
                b,
                span,
                value,
                &abitype::BoxedAbiType::Any.into(),
            ))
        }
        CapturesRepr::RecordStruct(record_struct) => {
            use crate::mir::ops::*;

            let field_regs = env_values
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

            Some(b.cast_boxed(span, record_reg, abitype::BoxedAbiType::Any))
        }
    }
}

/// Loads env values assuming all captured variables are still inside the local function
pub fn load_from_current_fun(
    local_values: &mut HashMap<hir::LocalId, Value>,
    env_values: &EnvValues,
) {
    local_values.extend(
        env_values
            .const_values
            .iter()
            .chain(env_values.free_values.iter())
            .map(|(local_id, value)| (*local_id, value.clone())),
    );
}

/// Loads environment values from an env parameter
pub fn load_from_env_param(
    b: &mut Builder,
    span: Span,
    local_values: &mut HashMap<hir::LocalId, Value>,
    env_values: &mut EnvValues,
    captures_reg: Option<BuiltReg>,
) {
    use crate::mir::value;
    use arret_runtime::abitype;

    // Include the const values directly
    local_values.extend(
        env_values
            .const_values
            .iter()
            .map(|(local_id, value)| (*local_id, value.clone())),
    );

    match &env_values.captures_repr {
        CapturesRepr::Empty => {}
        CapturesRepr::SingleBox => {
            let var_id = &env_values.free_values[0].0;
            let captures_reg = captures_reg.unwrap();
            let new_value: Value =
                value::RegValue::new(captures_reg, abitype::BoxedAbiType::Any.into()).into();

            local_values.insert(*var_id, new_value.clone());
            env_values.free_values[0].1 = new_value;
        }
        CapturesRepr::RecordStruct(record_struct) => {
            use crate::mir::ops::*;

            let record_reg: RegId = captures_reg.unwrap().into();

            for (field_index, (local_id, free_value)) in
                env_values.free_values.iter_mut().enumerate()
            {
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

                let new_value: Value = value::RegValue::new(field_reg, field_abi_type).into();

                local_values.insert(*local_id, new_value.clone());
                *free_value = new_value;
            }
        }
    }
}
