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
#[derive(Clone, Copy)]
pub struct Callback<F>
where
    F: Copy,
{
    captures: boxed::Captures,
    entry_point: F,
}

impl<F> Callback<F>
where
    F: Copy,
{
    /// Returns the captures for this callback
    pub fn captures(&self) -> boxed::Captures {
        self.captures
    }
}

/// Encoding of an entry point's ABI type
///
/// This is used internally by the compiler as a mechanism for reflecting the Rust function type.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct EntryPointAbiType {
    /// Types of the entry point's parameters
    ///
    /// This is not an [`abitype::ParamAbiType`] as captures should be determined by the callback
    /// implementation, not the callback's type.
    pub params: &'static [abitype::AbiType],

    /// Type of the entry point's return value
    pub ret: abitype::RetAbiType,
}

/// Trait used to encode a Rust function type as an [`EntryPointAbiType`]
pub trait EncodeEntryPointAbiType: Copy {
    /// Corresponding [`EntryPointAbiType`] for this Rust function type
    const ENTRY_POINT_ABI_TYPE: EntryPointAbiType;
}

macro_rules! define_generic_entry_point {
    ( $( $generic_param:ident ),* ) => {
        impl<R, $( $generic_param ),*> EncodeEntryPointAbiType for for<'s> extern "C" fn(&'s mut Task, captures: boxed::Captures, $( $generic_param ),* ) -> R
        where
            R: abitype::EncodeRetAbiType,
            $( $generic_param: abitype::EncodeAbiType ),*
        {
            const ENTRY_POINT_ABI_TYPE: EntryPointAbiType = EntryPointAbiType {
                params: &[$( $generic_param::ABI_TYPE ),*],
                ret: R::RET_ABI_TYPE,
            };
        }

        impl<R, $( $generic_param ),*> Callback<for<'s> extern "C" fn(&'s mut Task, captures: boxed::Captures, $( $generic_param ),* ) -> R>
        where
            R: abitype::EncodeRetAbiType,
            $( $generic_param: abitype::EncodeAbiType ),*
        {
            /// Applies this callback inside the given [`Task`] and returns its value
            ///
            /// It's important that the callback was created inside the passed `task` or undefined
            /// behaviour may result.
            #[allow(unused)]
            #[allow(non_snake_case)]
            #[allow(clippy::too_many_arguments)]
            pub fn apply(&self, task: &mut Task, $( $generic_param: $generic_param ),*) -> R {
                (self.entry_point)(task, self.captures, $( $generic_param ),*)
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
        let empty_abi_type = <extern "C" fn(&mut Task, boxed::Captures) as EncodeEntryPointAbiType>::ENTRY_POINT_ABI_TYPE;
        assert_eq!(abitype::RetAbiType::Void, empty_abi_type.ret);
        assert!(empty_abi_type.params.is_empty());

        let two_param_abi_type = <extern "C" fn(
            &mut Task,
            boxed::Captures,
            i64,
            Gc<boxed::Int>,
        ) -> bool as EncodeEntryPointAbiType>::ENTRY_POINT_ABI_TYPE;
        assert_eq!(
            abitype::AbiType::Bool.into_ret_abi_type(),
            two_param_abi_type.ret
        );
        assert_eq!(
            &[abitype::AbiType::Int, boxed::TypeTag::Int.into()],
            two_param_abi_type.params
        );
    }
}
