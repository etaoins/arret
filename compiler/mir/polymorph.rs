use std::iter;

use arret_runtime::abitype;
use arret_runtime::callback;

use crate::mir::ops;
use crate::ty;
use crate::ty::Ty;

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
    pub fn arret_fixed_params(&self) -> impl ExactSizeIterator<Item = &abitype::ABIType> {
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
            None => Ty::never().into(),
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

/// Recommends a polymorph ABI for a given list and ret type
pub fn polymorph_abi_for_list_ty<M: ty::PM>(
    has_closure: bool,
    list_ty: &ty::List<M>,
    ret_ty: &ty::Ref<M>,
) -> PolymorphABI {
    use crate::mir::specific_abi_type::*;

    let has_rest = list_ty.has_rest();

    let params = Some(abitype::BoxedABIType::Any.into())
        .filter(|_| has_closure)
        .into_iter()
        .chain(list_ty.fixed().iter().map(specific_abi_type_for_ty_ref))
        .chain(iter::once(abitype::TOP_LIST_BOXED_ABI_TYPE.into()).filter(|_| has_rest))
        .collect();

    let ops_abi = ops::OpsABI {
        params,
        ret: specific_ret_abi_type_for_ty_ref(ret_ty),
    };

    PolymorphABI {
        ops_abi,
        has_closure,
        has_rest: list_ty.has_rest(),
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
