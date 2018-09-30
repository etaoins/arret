use runtime::abitype::{ABIType, RetABIType};

/// Infers if a function can capture a parameter based on its return type
///
/// This is used for Rust functions where we don't have precise capture information. This uses a
/// very conservative algorithm where any function returning a box is assumed to capture all of its
/// arguments.
pub fn fun_may_capture_param(ret_abi_type: &RetABIType, param_abi_type: &ABIType) -> bool {
    let returns_box = match ret_abi_type {
        RetABIType::Inhabited(ABIType::Boxed(_)) => true,
        _ => false,
    };

    match param_abi_type {
        ABIType::Boxed(_) => returns_box,
        _ => true,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use runtime::boxed;

    #[test]
    fn fun_may_capture() {
        // Boxed return type can capture boxed parameter
        assert_eq!(
            true,
            fun_may_capture_param(&boxed::TypeTag::Int.into(), &boxed::TypeTag::Int.into())
        );

        // Unboxed return type cannot capture boxed parameter
        assert_eq!(
            false,
            fun_may_capture_param(&ABIType::Bool.into(), &boxed::TypeTag::Int.into())
        );
    }
}
