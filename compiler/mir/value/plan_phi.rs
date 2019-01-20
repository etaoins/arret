use runtime::abitype;

use crate::mir::value::Value;

pub fn plan_phi_abi_type(lhs: &Value, rhs: &Value) -> abitype::ABIType {
    use crate::mir::specific_abi_type::*;

    match (lhs, rhs) {
        (Value::Reg(lhs_reg_value), Value::Reg(rhs_reg_value))
            if lhs_reg_value.abi_type == rhs_reg_value.abi_type =>
        {
            // We have identical ABI types; this is easy
            rhs_reg_value.abi_type.clone()
        }
        (lhs, rhs) => {
            use std::iter;

            // We prefer working with unboxed values whenever possible. However, having to box a
            // value is much worse than working with a boxed temporary. Boxing requires calling in
            // to the allocator which is one of the most expensive things we can do.
            //
            // If both values are boxed then create an boxed phi. This prevents us from "wasting"
            // a box from prematurely unboxing it and then having to allocate to rebox it later.
            let both_boxed = [lhs, rhs].iter().all(|value| {
                match value {
                    Value::Const(_) => {
                        // Consts can be either boxed or unboxed
                        // This effectively means "whatever the other value wants"
                        true
                    }
                    Value::Reg(reg_value) => {
                        if let abitype::ABIType::Boxed(_) = reg_value.abi_type {
                            true
                        } else {
                            false
                        }
                    }
                    _ => true,
                }
            });

            let values_iter = iter::once(lhs).chain(iter::once(rhs));

            if both_boxed {
                specific_boxed_abi_type_for_values(values_iter).into()
            } else {
                specific_abi_type_for_values(values_iter)
            }
        }
    }
}
