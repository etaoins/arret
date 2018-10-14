use runtime::abitype;

use crate::mir::value::Value;
use crate::ty;

pub fn plan_phi_abi_type(lhs: &Value, rhs: &Value, poly: &ty::Poly) -> abitype::ABIType {
    use crate::mir::compact_abi_type::compact_abi_type_for_poly;

    match (lhs, rhs) {
        (Value::Reg(lhs_reg_value), Value::Reg(rhs_reg_value))
            if lhs_reg_value.abi_type == rhs_reg_value.abi_type =>
        {
            // We have identical ABI types; this is easy
            rhs_reg_value.abi_type.clone()
        }
        _ => compact_abi_type_for_poly(poly),
    }
}
