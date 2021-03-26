use arret_runtime::callback;
use arret_runtime::{abitype, boxed};

use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::Ty;

fn type_tag_to_ty<M: ty::Pm>(type_tag: boxed::TypeTag) -> Ty<M> {
    use arret_runtime::boxed::TypeTag;

    match type_tag {
        TypeTag::Float => Ty::Float,
        TypeTag::Char => Ty::Char,
        TypeTag::Str => Ty::Str,
        TypeTag::Sym => Ty::Sym,
        TypeTag::True => Ty::LitBool(true),
        TypeTag::False => Ty::LitBool(false),
        TypeTag::Int => Ty::Int,
        TypeTag::Vector => Ty::Vectorof(Box::new(Ty::Any.into())),
        TypeTag::Nil => ty::List::empty().into(),
        TypeTag::Pair => ty::List::new(Box::new([Ty::Any.into()]), Ty::Any.into()).into(),
        TypeTag::FunThunk => ty::TopFun::new(Purity::Impure.into(), Ty::Any.into()).into(),
        TypeTag::Record => Ty::TopRecord,
        TypeTag::Set => Ty::Set(Box::new(Ty::Any.into())),
        TypeTag::Map => Ty::Map(Box::new(ty::Map {
            key: Ty::Any.into(),
            value: Ty::Any.into(),
        })),
    }
}

pub trait ConvertableAbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M>;
    fn to_rust_str(&self) -> String;
}

impl ConvertableAbiType for abitype::AbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M> {
        use arret_runtime::abitype::AbiType;

        match self {
            AbiType::Bool => Ty::Bool.into(),
            AbiType::Char => Ty::Char.into(),
            AbiType::Float => Ty::Float.into(),
            AbiType::Int => Ty::Int.into(),
            AbiType::InternedSym => Ty::Sym.into(),
            AbiType::Boxed(boxed) => boxed.to_ty_ref(),
            AbiType::Callback(entry_point_abi) => entry_point_abi.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::AbiType;

        match self {
            AbiType::Bool => "bool".to_owned(),
            AbiType::Char => "char".to_owned(),
            AbiType::Float => "f64".to_owned(),
            AbiType::Int => "i64".to_owned(),
            AbiType::InternedSym => "InternedSym".to_owned(),
            AbiType::Boxed(boxed) => format!("Gc<{}>", boxed.to_rust_str()),
            AbiType::Callback(entry_point_abi) => entry_point_abi.to_rust_str(),
        }
    }
}

impl ConvertableAbiType for abitype::BoxedAbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M> {
        use arret_runtime::abitype::BoxedAbiType;

        match self {
            BoxedAbiType::Any => Ty::Any.into(),
            BoxedAbiType::Vector(member) => Ty::Vectorof(Box::new(member.to_ty_ref())).into(),
            BoxedAbiType::Set(member) => Ty::Set(Box::new(member.to_ty_ref())).into(),
            BoxedAbiType::Map(key, value) => Ty::Map(Box::new(ty::Map {
                key: key.to_ty_ref(),
                value: value.to_ty_ref(),
            }))
            .into(),
            BoxedAbiType::List(member) => ty::List::new_uniform(member.to_ty_ref()).into(),
            BoxedAbiType::Pair(member) => {
                let member_ty_ref: ty::Ref<M> = member.to_ty_ref();
                ty::List::new(Box::new([member_ty_ref.clone()]), member_ty_ref).into()
            }
            BoxedAbiType::UniqueTagged(type_tag) => type_tag_to_ty(*type_tag).into(),
            BoxedAbiType::Union(_, tags) => {
                let members = tags.iter().map(|type_tag| type_tag_to_ty(*type_tag).into());

                ty::unify::unify_ty_ref_iter(members)
            }
        }
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::BoxedAbiType;

        match self {
            BoxedAbiType::Any => "boxed::Any".to_owned(),
            BoxedAbiType::Vector(member) => format!("boxed::Vector<{}>", member.to_rust_str()),
            BoxedAbiType::List(member) => format!("boxed::List<{}>", member.to_rust_str()),
            BoxedAbiType::Pair(member) => format!("boxed::Pair<{}>", member.to_rust_str()),
            BoxedAbiType::Set(member) => format!("boxed::Set<{}>", member.to_rust_str()),
            BoxedAbiType::UniqueTagged(type_tag) => format!("boxed::{}", type_tag.to_str()),
            BoxedAbiType::Union(name, _) => format!("boxed::{}", name),
            BoxedAbiType::Map(key, value) => {
                format!("boxed::Map<{}, {}>", key.to_rust_str(), value.to_rust_str())
            }
        }
    }
}

impl ConvertableAbiType for abitype::RetAbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M> {
        use arret_runtime::abitype::RetAbiType;

        match self {
            RetAbiType::Void => Ty::unit().into(),
            RetAbiType::Never => Ty::never().into(),
            RetAbiType::Inhabited(abi_type) => abi_type.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::RetAbiType;

        match self {
            RetAbiType::Void => "()".to_owned(),
            RetAbiType::Never => "Never".to_owned(),
            RetAbiType::Inhabited(abi_type) => abi_type.to_rust_str(),
        }
    }
}

impl ConvertableAbiType for abitype::ParamAbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M> {
        self.abi_type.to_ty_ref()
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::AbiType;
        use arret_runtime::abitype::ParamCapture;

        match &self.abi_type {
            AbiType::Boxed(boxed) => match self.capture {
                ParamCapture::Auto => format!("Gc<{}>", boxed.to_rust_str()),
                ParamCapture::Never => format!("NoCapture<{}>", boxed.to_rust_str()),
                ParamCapture::Always => format!("Capture<{}>", boxed.to_rust_str()),
            },
            other => other.to_rust_str(),
        }
    }
}

impl ConvertableAbiType for callback::EntryPointAbiType {
    fn to_ty_ref<M: ty::Pm>(&self) -> ty::Ref<M> {
        // TODO: How do we deal with rest params?
        let fixed_param_ty_refs = self
            .params
            .iter()
            .map(ConvertableAbiType::to_ty_ref)
            .collect();

        ty::Fun::new_mono(
            ty::List::new_tuple(fixed_param_ty_refs),
            Purity::Impure.into(),
            self.ret.to_ty_ref(),
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
            "extern \"C\" fn(&mut Task, boxed::Captures{}) -> {}",
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
        use arret_runtime::abitype::EncodeBoxedAbiType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Pair<boxed::Int> as EncodeBoxedAbiType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Pair<boxed::Int>", boxed_abi_type.to_rust_str());

        let int_pair_poly: ty::Ref<ty::Poly> =
            ty::List::new(Box::new([Ty::Int.into()]), Ty::Int.into()).into();

        assert_eq!(int_pair_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn bool_abi_type() {
        use arret_runtime::abitype::EncodeBoxedAbiType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Bool as EncodeBoxedAbiType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Bool", boxed_abi_type.to_rust_str());

        let bool_poly: ty::Ref<ty::Poly> = Ty::Bool.into();
        assert_eq!(bool_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn nil_abi_type() {
        use arret_runtime::abitype::EncodeBoxedAbiType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Nil as EncodeBoxedAbiType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Nil", boxed_abi_type.to_rust_str());

        let nil_poly: ty::Ref<ty::Poly> = ty::List::empty().into();
        assert_eq!(nil_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn callback_abi_type() {
        use arret_runtime::task;

        let entry_point_abi_type = <extern "C" fn(&mut task::Task, boxed::Captures, i64) -> char as callback::EncodeEntryPointAbiType>::ENTRY_POINT_ABI_TYPE;

        assert_eq!(
            "extern \"C\" fn(&mut Task, boxed::Captures, i64) -> char",
            entry_point_abi_type.to_rust_str()
        );

        let arret_poly: ty::Ref<ty::Poly> = ty::Fun::new_mono(
            ty::List::new_tuple(Box::new([Ty::Int.into()])),
            Purity::Impure.into(),
            Ty::Char.into(),
        )
        .into();

        assert_eq!(arret_poly, entry_point_abi_type.to_ty_ref());
    }

    #[test]
    fn captured_int_abi_type() {
        use arret_runtime::abitype::{EncodeBoxedAbiType, ParamAbiType};
        use arret_runtime::boxed;

        let param_abi_type = ParamAbiType {
            abi_type: <boxed::Int as EncodeBoxedAbiType>::BOXED_ABI_TYPE.into(),
            capture: abitype::ParamCapture::Always,
        };

        assert_eq!("Capture<boxed::Int>", param_abi_type.to_rust_str());

        let int_poly: ty::Ref<ty::Poly> = Ty::Int.into();
        assert_eq!(int_poly, param_abi_type.to_ty_ref());
    }
}
