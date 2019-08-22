use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::boxed::TypeTag;
use arret_runtime::class_map;
use arret_runtime::intern::InternedSym;

use crate::mir::eval_hir::{EvalHirCtx, EvaledRecordClass};
use crate::mir::value::Value;
use crate::ty::record;

#[allow(clippy::cast_ptr_alignment)]
fn record_to_const(
    ehx: &mut EvalHirCtx,
    record_cons: &record::ConsId,
    field_values: &[Value],
) -> Option<Gc<boxed::Any>> {
    let EvaledRecordClass {
        jit_record_class_id,
        jit_data_layout,
        ..
    } = *ehx.evaled_record_class_for_cons(record_cons);

    let classmap_class = ehx
        .as_heap()
        .type_info()
        .class_map()
        .class_for_record_class_id(jit_record_class_id);

    let data = boxed::RecordData::alloc(jit_data_layout);

    let classmap_fields: Vec<class_map::Field> = classmap_class.field_iter().collect();
    for (classmap_field, field_value) in classmap_fields.iter().zip(field_values.iter()) {
        unsafe {
            use class_map::FieldType;
            let field_ptr = data.as_ptr().add(classmap_field.offset());

            match classmap_field.field_type() {
                FieldType::Bool => {
                    let bool_ref = &mut *(field_ptr as *mut bool);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *bool_ref = boxed_field
                        .downcast_ref::<boxed::Bool>()
                        .expect("unexpected value field type while boxing constant bool")
                        .as_bool();
                }
                FieldType::Int => {
                    let int_ref = &mut *(field_ptr as *mut i64);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *int_ref = boxed_field
                        .downcast_ref::<boxed::Int>()
                        .expect("unexpected value field type while boxing constant int")
                        .value();
                }
                FieldType::Float => {
                    let float_ref = &mut *(field_ptr as *mut f64);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *float_ref = boxed_field
                        .downcast_ref::<boxed::Float>()
                        .expect("unexpected value field type while boxing constant float")
                        .value();
                }
                FieldType::Char => {
                    let char_ref = &mut *(field_ptr as *mut char);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *char_ref = boxed_field
                        .downcast_ref::<boxed::Char>()
                        .expect("unexpected value field type while boxing constant char")
                        .value();
                }
                FieldType::InternedSym => {
                    let interned_sym_ref = &mut *(field_ptr as *mut InternedSym);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *interned_sym_ref = boxed_field
                        .downcast_ref::<boxed::Sym>()
                        .expect("unexpected value field type while boxing constant interned sym")
                        .interned();
                }
                FieldType::Boxed => {
                    let boxed_ref = &mut *(field_ptr as *mut Gc<boxed::Any>);
                    let boxed_field = value_to_const(ehx, field_value)?;

                    *boxed_ref = boxed_field;
                }
            }
        }
    }

    Some(boxed::Record::new(ehx, jit_record_class_id, data).as_any_ref())
}

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
        Value::Record(record_cons, field_values) => record_to_const(ehx, record_cons, field_values),
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
        Value::RecordCons(cons) => {
            let record_cons_arret_fun = ehx.synthetic_funs().record_cons_arret_fun(cons).clone();

            ehx.arret_fun_to_jit_boxed(&record_cons_arret_fun)
                .map(|f| f.as_any_ref())
        }
        Value::FieldAccessor(cons, field_index) => {
            let field_accessor_arret_fun = ehx
                .synthetic_funs()
                .field_accessor_arret_fun(cons, *field_index)
                .clone();

            ehx.arret_fun_to_jit_boxed(&field_accessor_arret_fun)
                .map(|f| f.as_any_ref())
        }
        Value::RustFun(ref rust_fun) => {
            Some(ehx.rust_fun_to_jit_boxed(rust_fun.clone()).as_any_ref())
        }
        Value::Reg(ref reg_value) => {
            if reg_value.possible_type_tags == TypeTag::Nil.into() {
                Some(boxed::NIL_INSTANCE.as_any_ref())
            } else if reg_value.possible_type_tags == TypeTag::True.into() {
                Some(boxed::TRUE_INSTANCE.as_any_ref())
            } else if reg_value.possible_type_tags == TypeTag::False.into() {
                Some(boxed::FALSE_INSTANCE.as_any_ref())
            } else {
                None
            }
        }
    }
}
