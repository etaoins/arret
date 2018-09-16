use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::ops::RegId;
use crate::mir::value::Value;

fn const_to_reg(
    b: &mut Builder,
    span: Span,
    any_ref: Gc<boxed::Any>,
    abi_type: &abitype::ABIType,
) -> RegId {
    use crate::mir::ops::*;

    let subtype = any_ref.as_subtype();

    match (subtype, abi_type) {
        (boxed::AnySubtype::Int(int_ref), abitype::ABIType::Int) => {
            b.push_reg(span, OpKind::ConstInt, int_ref.value())
        }
        (subtype, abi_type) => unimplemented!(
            "Unimplemented const {:?} to reg {:?} conversion",
            subtype,
            abi_type
        ),
    }
}

pub fn value_to_reg(
    b: &mut Builder,
    span: Span,
    value: &Value,
    abi_type: &abitype::ABIType,
) -> RegId {
    match value {
        Value::Const(any_ref) => const_to_reg(b, span, *any_ref, abi_type),
        _ => unimplemented!("Unimplemented value to reg conversion"),
    }
}
