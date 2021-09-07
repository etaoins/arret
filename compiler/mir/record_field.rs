use arret_syntax::span::Span;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::TryToBuilder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty::record;

pub fn load_record_field(
    ehx: &mut EvalHirCtx,
    b: &mut impl TryToBuilder,
    span: Span,
    record_cons: &record::ConsId,
    record_value: &Value,
    field_index: usize,
) -> Value {
    match record_value {
        Value::Record(_, fields) => fields[field_index].clone(),
        Value::Const(boxed_any) => {
            use boxed::FieldValue;

            let boxed_record = boxed_any
                .downcast_ref::<boxed::Record>()
                .expect("unexpected type when accessing record field");

            match boxed_record
                .field_values(ehx.as_heap())
                .nth(field_index)
                .unwrap()
            {
                FieldValue::Bool(bool_value) => boxed::Bool::singleton_ref(bool_value).into(),
                FieldValue::Int(int_value) => boxed::Int::new(ehx, int_value).into(),
                FieldValue::Float(float_value) => boxed::Float::new(ehx, float_value).into(),
                FieldValue::Char(char_value) => boxed::Char::new(ehx, char_value).into(),
                FieldValue::Boxed(boxed_any) => boxed_any.into(),
                FieldValue::InternedSym(interned) => {
                    boxed::Sym::from_interned_sym(ehx, interned).into()
                }
            }
        }
        other_value => {
            use crate::mir::ops::*;
            use crate::mir::value::build_reg::value_to_reg;

            let record_struct = ehx
                .evaled_record_class_for_cons(record_cons)
                .record_struct
                .clone();

            let b = if let Some(b) = b.try_to_builder() {
                b
            } else {
                panic!("need builder to access field of boxed record reg");
            };

            let record_reg =
                value_to_reg(ehx, b, span, other_value, &boxed::TypeTag::Record.into());

            let field_reg = b.push_reg(
                span,
                OpKind::LoadBoxedRecordField,
                LoadBoxedRecordFieldOp {
                    field_index,
                    record_reg: record_reg.into(),
                    record_struct: record_struct.clone(),
                },
            );

            let field_abi_type = record_struct.field_abi_types[field_index].clone();
            value::RegValue::new(field_reg, field_abi_type).into()
        }
    }
}
