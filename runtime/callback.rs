#![warn(missing_docs)]

//! Typed callback functions

use crate::abitype;
use crate::boxed;
use crate::task::Task;

/// Typed callback function
///
/// This is typically when taking a callback as a parameter to an RFI function. This is not a
/// proper boxed value and can neither be stored in a collection or returned as a value. For those
/// cases [`boxed::FunThunk`] should be used instead.
#[repr(C)]
pub struct Callback<F>
where
    F: EncodeEntryPointABIType,
{
    closure: boxed::Closure,
    entry_point: F,
}

impl<F> Callback<F>
where
    F: EncodeEntryPointABIType,
{
    /// Returns the closure for this callback
    pub fn closure(&self) -> boxed::Closure {
        self.closure
    }

    /// Returns a mutable reference to the closure
    pub(crate) fn closure_mut(&mut self) -> &mut boxed::Closure {
        &mut self.closure
    }

    /// Returns the entry point to the callback's implementation
    ///
    /// This would typically be used via `apply`.
    pub fn entry_point(&self) -> F {
        self.entry_point
    }
}

/// Encoding of an entry point's ABI type
///
/// This is used internally by the compiler as a mechanism for reflecting the Rust function type.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct EntryPointABIType {
    /// Types of the entry point's parameters
    ///
    /// This is not an [`abitype::ParamABIType`] as captures should be determined by the callback
    /// implementation, not the callback's type.
    pub params: &'static [abitype::ABIType],

    /// Type of the entry point's return value
    pub ret: abitype::RetABIType,
}

/// Trait used to encode a Rust function type as an [`EntryPointABIType`]
pub trait EncodeEntryPointABIType: Copy {
    /// Corresponding [`EntryPointABIType`] for this Rust function type
    const ENTRY_POINT_ABI_TYPE: EntryPointABIType;
}

macro_rules! define_generic_entry_point {
    ( $( $generic_param:ident ),* ) => {
        impl<R, $( $generic_param ),*> EncodeEntryPointABIType for for<'s> extern "C" fn(&'s mut Task, closure: boxed::Closure, $( $generic_param ),* ) -> R
        where
            R: abitype::EncodeRetABIType,
            $( $generic_param: abitype::EncodeABIType ),*
        {
            const ENTRY_POINT_ABI_TYPE: EntryPointABIType = EntryPointABIType {
                params: &[$( $generic_param::ABI_TYPE ),*],
                ret: R::RET_ABI_TYPE,
            };
        }

        impl<R, $( $generic_param ),*> Callback<for<'s> extern "C" fn(&'s mut Task, closure: boxed::Closure, $( $generic_param ),* ) -> R>
        where
            R: abitype::EncodeRetABIType,
            $( $generic_param: abitype::EncodeABIType ),*
        {
            /// Applies this callback inside the given [`Task`] and returns its value
            ///
            /// It's important that the callback was created inside the passed `task` or undefined
            /// behaviour may result.
            #[allow(unused)]
            #[allow(non_snake_case)]
            #[allow(clippy::too_many_arguments)]
            pub fn apply(&self, task: &mut Task, $( $generic_param: $generic_param ),*) -> R {
                (self.entry_point)(task, self.closure, $( $generic_param ),*)
            }
        }
    }
}

define_generic_entry_point!();
define_generic_entry_point!(A);
define_generic_entry_point!(A, B);
define_generic_entry_point!(A, B, C);
define_generic_entry_point!(A, B, C, D);
define_generic_entry_point!(A, B, C, D, E);
define_generic_entry_point!(A, B, C, D, E, F);
define_generic_entry_point!(A, B, C, D, E, F, G);
define_generic_entry_point!(A, B, C, D, E, F, G, H);

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::refs::*;

    #[test]
    fn encode_entry_point_abi_type() {
        let empty_abi_type = <extern "C" fn(&mut Task, boxed::Closure) as EncodeEntryPointABIType>::ENTRY_POINT_ABI_TYPE;
        assert_eq!(abitype::RetABIType::Void, empty_abi_type.ret);
        assert!(empty_abi_type.params.is_empty());

        let two_param_abi_type = <extern "C" fn(
            &mut Task,
            boxed::Closure,
            i64,
            Gc<boxed::Int>,
        ) -> bool as EncodeEntryPointABIType>::ENTRY_POINT_ABI_TYPE;
        assert_eq!(
            abitype::ABIType::Bool.into_ret_abi_type(),
            two_param_abi_type.ret
        );
        assert_eq!(
            &[abitype::ABIType::Int, boxed::TypeTag::Int.into()],
            two_param_abi_type.params
        );
    }
}
