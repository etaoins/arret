use std::iter;

use arret_runtime::abitype;
use arret_runtime::callback;

use crate::mir::ops;
use crate::mir::value::Value;
use crate::ty;

/// PolymorphABI annotates OpsABI with information about if a function expects a closure or rest
///
/// This is information that's useful while generating MIR but can be discarded when building Ops.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct PolymorphABI {
    pub ops_abi: ops::OpsABI,
    pub has_closure: bool,
    pub has_rest: bool,
}

impl PolymorphABI {
    pub fn thunk_abi() -> PolymorphABI {
        PolymorphABI {
            ops_abi: ops::OpsABI::thunk_abi(),
            has_closure: true,
            has_rest: true,
        }
    }

    /// Returns the ABI types of the params corresponding to Arret fixed params
    pub fn arret_fixed_params(&self) -> impl DoubleEndedIterator<Item = &abitype::ABIType> {
        let mut ops_param_iter = self.ops_abi.params.iter();

        if self.has_closure {
            ops_param_iter.next().unwrap();
        }

        if self.has_rest {
            ops_param_iter.next_back().unwrap();
        }

        ops_param_iter
    }

    /// Returns the ABI type of the param corresponding to the Arret rest param, if any
    pub fn arret_rest_param(&self) -> Option<&abitype::ABIType> {
        if self.has_rest {
            self.ops_abi.params.last()
        } else {
            None
        }
    }

    /// Returns the Arret type for our parameter list
    pub fn param_ty_ref<M: ty::PM>(&self) -> ty::List<M> {
        use crate::ty::conv_abi::ConvertableABIType;

        let fixed_refs = self
            .arret_fixed_params()
            .map(ConvertableABIType::to_ty_ref)
            .collect();

        let rest_ref = match self.arret_rest_param() {
            Some(abitype::ABIType::Boxed(abitype::BoxedABIType::List(memeber_abi_type))) => {
                memeber_abi_type.to_ty_ref()
            }
            Some(other) => {
                panic!("cannot determine member type for ABI rest list {:?}", other);
            }
            None => ty::Ty::never().into(),
        };

        // If our rest type uses a Pair this can have fixed members
        ty::List::new(fixed_refs, rest_ref)
    }
}

impl From<callback::EntryPointABIType> for PolymorphABI {
    fn from(abi_type: callback::EntryPointABIType) -> Self {
        PolymorphABI {
            ops_abi: abi_type.into(),
            has_closure: true,
            has_rest: false,
        }
    }
}

// This is essentially the thunk ABI type with an optional closure
fn fallback_polymorph_abi(has_closure: bool, ret_ty: &ty::Ref<ty::Mono>) -> PolymorphABI {
    use crate::mir::specific_abi_type::specific_abi_type_for_ty_ref;

    let params = Some(abitype::BoxedABIType::Any.into())
        .filter(|_| has_closure)
        .into_iter()
        .chain(iter::once(abitype::TOP_LIST_BOXED_ABI_TYPE.into()))
        .collect();

    let ops_abi = ops::OpsABI {
        params,
        ret: specific_abi_type_for_ty_ref(ret_ty).into(),
    };

    PolymorphABI {
        ops_abi,
        has_closure,
        has_rest: true,
    }
}

// TODO: This should use the list iterator so we can deal with chained lists, consts, etc
fn list_polymorph_abi(
    has_closure: bool,
    fixed: &[Value],
    rest: Option<&Value>,
    ret_ty: &ty::Ref<ty::Mono>,
) -> PolymorphABI {
    use crate::mir::specific_abi_type::{
        specific_abi_type_for_ty_ref, specific_abi_type_for_value,
    };

    let params = Some(abitype::BoxedABIType::Any.into())
        .filter(|_| has_closure)
        .into_iter()
        .chain(fixed.iter().map(specific_abi_type_for_value))
        .chain(
            rest.into_iter()
                .map(|_| abitype::TOP_LIST_BOXED_ABI_TYPE.into()),
        )
        .collect();

    let ops_abi = ops::OpsABI {
        params,
        ret: specific_abi_type_for_ty_ref(ret_ty).into(),
    };

    PolymorphABI {
        ops_abi,
        has_closure,
        has_rest: rest.is_some(),
    }
}

/// Recommends a polymorph ABI for a given list value and ret type
pub fn polymorph_abi_for_arg_list_value(
    has_closure: bool,
    arg_list_value: &Value,
    ret_ty: &ty::Ref<ty::Mono>,
) -> PolymorphABI {
    match arg_list_value {
        Value::List(fixed, rest) => {
            let rest = match rest {
                Some(value) => Some(value.as_ref()),
                None => None,
            };

            list_polymorph_abi(has_closure, fixed.as_ref(), rest, ret_ty)
        }
        _ => {
            // This isn't a list; we can't do anything useful with it
            fallback_polymorph_abi(has_closure, ret_ty)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir;

    #[test]
    fn polymorph_abi_param_ty_ref() {
        use arret_runtime::abitype::EncodeBoxedABIType;
        use arret_runtime::boxed;

        let thunk_param_poly = PolymorphABI::thunk_abi().param_ty_ref();
        let expected_poly = hir::poly_for_str("(List & Any)");
        assert_eq!(expected_poly, thunk_param_poly.into());

        let mul_param_poly = PolymorphABI {
            ops_abi: ops::OpsABI {
                params: Box::new([
                    boxed::Num::BOXED_ABI_TYPE.into(),
                    abitype::BoxedABIType::List(&boxed::Num::BOXED_ABI_TYPE).into(),
                ]),
                ret: boxed::Num::BOXED_ABI_TYPE.into(),
            },
            has_closure: false,
            has_rest: true,
        }
        .param_ty_ref();

        let expected_poly = hir::poly_for_str("(List Num & Num)");
        assert_eq!(expected_poly, mul_param_poly.into());
    }
}
