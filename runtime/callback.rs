use crate::abitype;
use crate::boxed;
use crate::task::Task;

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
    pub fn new(closure: boxed::Closure, entry_point: F) -> Callback<F> {
        Callback {
            closure,
            entry_point,
        }
    }

    pub fn closure(&self) -> boxed::Closure {
        self.closure
    }

    pub fn entry_point(&self) -> F {
        self.entry_point
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EntryPointABIType {
    /// Types of the entry point's parameters
    ///
    /// This is not a ParamABIType as captures should be determined by the callback implementation,
    /// not the callback's type.
    pub params: &'static [abitype::ABIType],

    pub ret: abitype::RetABIType,
}

pub trait EncodeEntryPointABIType: Copy {
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

        let two_param_abi_type = <extern "C" fn(&mut Task, boxed::Closure, i64, Gc<boxed::Int>)
            -> bool as EncodeEntryPointABIType>::ENTRY_POINT_ABI_TYPE;
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
