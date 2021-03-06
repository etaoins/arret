use arret_runtime::abitype;
use arret_runtime::callback;

use crate::mir::ops;
use crate::ty;
use crate::ty::Ty;

/// PolymorphAbi annotates OpsAbi with information about if a function expects a captures or rest
///
/// This is information that's useful while generating MIR but can be discarded when building Ops.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct PolymorphAbi {
    pub call_conv: ops::CallConv,

    pub has_captures: bool,
    pub fixed_params: Box<[abitype::AbiType]>,
    pub rest_param: Option<abitype::AbiType>,

    pub ret: abitype::RetAbiType,
}

impl PolymorphAbi {
    pub fn thunk_abi() -> PolymorphAbi {
        PolymorphAbi {
            call_conv: ops::CallConv::Ccc,

            has_captures: true,
            fixed_params: Box::new([]),
            rest_param: Some(abitype::TOP_LIST_BOXED_ABI_TYPE.into()),

            ret: abitype::BoxedAbiType::Any.into(),
        }
    }

    /// Returns the Arret type for our parameter list
    pub fn param_ty_ref<M: ty::Pm>(&self) -> ty::List<M> {
        use crate::ty::conv_abi::ConvertableAbiType;

        let fixed_refs = self
            .fixed_params
            .iter()
            .map(ConvertableAbiType::to_ty_ref)
            .collect();

        let rest_ref = match &self.rest_param {
            Some(abitype::AbiType::Boxed(abitype::BoxedAbiType::List(member_abi_type))) => {
                member_abi_type.to_ty_ref()
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

impl From<callback::EntryPointAbiType> for PolymorphAbi {
    fn from(abi_type: callback::EntryPointAbiType) -> Self {
        PolymorphAbi {
            call_conv: ops::CallConv::Ccc,

            has_captures: true,
            fixed_params: abi_type.params.iter().cloned().collect(),
            rest_param: None,

            ret: abi_type.ret,
        }
    }
}

impl From<PolymorphAbi> for ops::OpsAbi {
    fn from(polymorph_abi: PolymorphAbi) -> Self {
        ops::OpsAbi {
            params: Some(abitype::BoxedAbiType::Any.into())
                .filter(|_| polymorph_abi.has_captures)
                .into_iter()
                .chain(polymorph_abi.fixed_params.iter().cloned())
                .chain(polymorph_abi.rest_param.iter().cloned())
                .collect(),

            call_conv: polymorph_abi.call_conv,
            ret: polymorph_abi.ret,
        }
    }
}

/// Recommends a polymorph ABI for a given list and ret type
pub fn polymorph_abi_for_list_ty<M: ty::Pm>(
    has_captures: bool,
    list_ty: &ty::List<M>,
    ret_ty: &ty::Ref<M>,
) -> PolymorphAbi {
    use crate::mir::specific_abi_type::*;

    PolymorphAbi {
        call_conv: ops::CallConv::FastCc,

        has_captures,

        fixed_params: list_ty
            .fixed()
            .iter()
            .map(specific_abi_type_for_ty_ref)
            .collect(),

        rest_param: Some(
            abitype::BoxedAbiType::List(specific_boxed_abi_type_for_ty_ref(list_ty.rest())).into(),
        )
        .filter(|_| list_ty.has_rest()),

        ret: specific_ret_abi_type_for_ty_ref(ret_ty),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir;

    #[test]
    fn polymorph_abi_param_ty_ref() {
        use arret_runtime::abitype::EncodeBoxedAbiType;
        use arret_runtime::boxed;

        let thunk_param_poly = PolymorphAbi::thunk_abi().param_ty_ref();
        let expected_poly = hir::poly_for_str("(List & Any)");
        assert_eq!(expected_poly, thunk_param_poly.into());

        let mul_param_poly = PolymorphAbi {
            call_conv: ops::CallConv::FastCc,

            has_captures: false,
            fixed_params: Box::new([boxed::Num::BOXED_ABI_TYPE.into()]),
            rest_param: Some(abitype::BoxedAbiType::List(&boxed::Num::BOXED_ABI_TYPE).into()),

            ret: boxed::Num::BOXED_ABI_TYPE.into(),
        }
        .param_ty_ref();

        let expected_poly = hir::poly_for_str("(List Num & Num)");
        assert_eq!(expected_poly, mul_param_poly.into());
    }
}
