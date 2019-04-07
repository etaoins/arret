use runtime::callback;
use runtime::{abitype, boxed};

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

fn type_tag_to_ty<M: ty::PM>(type_tag: boxed::TypeTag) -> ty::Ty<M> {
    use runtime::boxed::TypeTag;

    match type_tag {
        TypeTag::Float => ty::Ty::Float,
        TypeTag::Char => ty::Ty::Char,
        TypeTag::Str => ty::Ty::Str,
        TypeTag::Sym => ty::Ty::Sym,
        TypeTag::True => ty::Ty::LitBool(true),
        TypeTag::False => ty::Ty::LitBool(false),
        TypeTag::Int => ty::Ty::Int,
        TypeTag::Vector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into())),
        TypeTag::Nil => ty::List::empty().into(),
        TypeTag::Pair => ty::List::new(Box::new([ty::Ty::Any.into()]), ty::Ty::Any.into()).into(),
        TypeTag::FunThunk => ty::TopFun::new(Purity::Impure.into(), ty::Ty::Any.into()).into(),
    }
}

pub trait ConvertableABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M>;
    fn to_rust_str(&self) -> String;
}

impl ConvertableABIType for abitype::ABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        use runtime::abitype::ABIType;

        match self {
            ABIType::Bool => ty::Ty::Bool.into(),
            ABIType::Char => ty::Ty::Char.into(),
            ABIType::Float => ty::Ty::Float.into(),
            ABIType::Int => ty::Ty::Int.into(),
            ABIType::Boxed(boxed) => boxed.to_ty_ref(),
            ABIType::Callback(entry_point_abi) => entry_point_abi.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use runtime::abitype::ABIType;

        match self {
            ABIType::Bool => "bool".to_owned(),
            ABIType::Char => "char".to_owned(),
            ABIType::Float => "f64".to_owned(),
            ABIType::Int => "i64".to_owned(),
            ABIType::Boxed(boxed) => format!("Gc<{}>", boxed.to_rust_str()),
            ABIType::Callback(entry_point_abi) => entry_point_abi.to_rust_str(),
        }
    }
}

impl ConvertableABIType for abitype::BoxedABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        use runtime::abitype::BoxedABIType;

        match self {
            BoxedABIType::Any => ty::Ty::Any.into(),
            BoxedABIType::Vector(member) => ty::Ty::Vectorof(Box::new(member.to_ty_ref())).into(),
            BoxedABIType::List(member) => ty::List::new(Box::new([]), member.to_ty_ref()).into(),
            BoxedABIType::Pair(member) => {
                let member_ty_ref: ty::Ref<M> = member.to_ty_ref();
                ty::List::new(Box::new([member_ty_ref.clone()]), member_ty_ref).into()
            }
            BoxedABIType::UniqueTagged(type_tag) => type_tag_to_ty(*type_tag).into(),
            BoxedABIType::Union(_, tags) => {
                let members = tags.iter().map(|type_tag| type_tag_to_ty(*type_tag).into());

                ty::unify::unify_ty_ref_iter(members)
            }
        }
    }

    fn to_rust_str(&self) -> String {
        use runtime::abitype::BoxedABIType;

        match self {
            BoxedABIType::Any => "boxed::Any".to_owned(),
            BoxedABIType::Vector(member) => format!("boxed::Vector<{}>", member.to_rust_str()),
            BoxedABIType::List(member) => format!("boxed::List<{}>", member.to_rust_str()),
            BoxedABIType::Pair(member) => format!("boxed::Pair<{}>", member.to_rust_str()),
            BoxedABIType::UniqueTagged(type_tag) => format!("boxed::{}", type_tag.to_str()),
            BoxedABIType::Union(name, _) => format!("boxed::{}", name),
        }
    }
}

impl ConvertableABIType for abitype::RetABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        use runtime::abitype::RetABIType;

        match self {
            RetABIType::Void => ty::Ty::unit().into(),
            RetABIType::Never => ty::Ty::never().into(),
            RetABIType::Inhabited(abi_type) => abi_type.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use runtime::abitype::RetABIType;

        match self {
            RetABIType::Void => "()".to_owned(),
            RetABIType::Never => "Never".to_owned(),
            RetABIType::Inhabited(abi_type) => abi_type.to_rust_str(),
        }
    }
}

impl ConvertableABIType for callback::EntryPointABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        let top_fun_ty = ty::TopFun::new(Purity::Impure.into(), self.ret.to_ty_ref());

        // TODO: How do we deal with rest params?
        let fixed_param_ty_refs = self
            .params
            .iter()
            .map(ConvertableABIType::to_ty_ref)
            .collect();

        let param_list_ty = ty::List::new(fixed_param_ty_refs, ty::Ty::never().into());

        ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            top_fun_ty,
            param_list_ty,
        )
        .into()
    }

    fn to_rust_str(&self) -> String {
        let params_str = if self.params.is_empty() {
            "".to_owned()
        } else {
            self.params
                .iter()
                .map(|abi_type| format!(", {}", abi_type.to_rust_str()))
                .collect::<Vec<String>>()
                .join("")
        };

        format!(
            "extern \"C\" fn(&mut Task, boxed::Closure{}) -> {}",
            params_str,
            self.ret.to_rust_str()
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pair_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Pair<boxed::Int> as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Pair<boxed::Int>", boxed_abi_type.to_rust_str());

        let int_pair_poly: ty::Ref<ty::Poly> =
            ty::List::new(Box::new([ty::Ty::Int.into()]), ty::Ty::Int.into()).into();

        assert_eq!(int_pair_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn bool_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Bool as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Bool", boxed_abi_type.to_rust_str());

        let bool_poly: ty::Ref<ty::Poly> = ty::Ty::Bool.into();
        assert_eq!(bool_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn nil_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Nil as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Nil", boxed_abi_type.to_rust_str());

        let nil_poly: ty::Ref<ty::Poly> = ty::List::empty().into();
        assert_eq!(nil_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn callback_abi_type() {
        use runtime::task;

        let entry_point_abi_type = <extern "C" fn(&mut task::Task, boxed::Closure, i64) -> char as callback::EncodeEntryPointABIType>::ENTRY_POINT_ABI_TYPE;

        assert_eq!(
            "extern \"C\" fn(&mut Task, boxed::Closure, i64) -> char",
            entry_point_abi_type.to_rust_str()
        );

        let arret_poly: ty::Ref<ty::Poly> = ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            ty::TopFun::new(Purity::Impure.into(), ty::Ty::Char.into()),
            ty::List::new(Box::new([ty::Ty::Int.into()]), ty::Ty::never().into()),
        )
        .into();

        assert_eq!(arret_poly, entry_point_abi_type.to_ty_ref());
    }
}
