use arret_runtime::callback;
use arret_runtime::{abitype, boxed};

use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::Ty;

fn type_tag_to_ty<M: ty::PM>(type_tag: boxed::TypeTag) -> Ty<M> {
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
    }
}

pub trait ConvertableABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M>;
    fn to_rust_str(&self) -> String;
}

impl ConvertableABIType for abitype::ABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        use arret_runtime::abitype::ABIType;

        match self {
            ABIType::Bool => Ty::Bool.into(),
            ABIType::Char => Ty::Char.into(),
            ABIType::Float => Ty::Float.into(),
            ABIType::Int => Ty::Int.into(),
            ABIType::InternedSym => Ty::Sym.into(),
            ABIType::Boxed(boxed) => boxed.to_ty_ref(),
            ABIType::Callback(entry_point_abi) => entry_point_abi.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::ABIType;

        match self {
            ABIType::Bool => "bool".to_owned(),
            ABIType::Char => "char".to_owned(),
            ABIType::Float => "f64".to_owned(),
            ABIType::Int => "i64".to_owned(),
            ABIType::InternedSym => "InternedSym".to_owned(),
            ABIType::Boxed(boxed) => format!("Gc<{}>", boxed.to_rust_str()),
            ABIType::Callback(entry_point_abi) => entry_point_abi.to_rust_str(),
        }
    }
}

impl ConvertableABIType for abitype::BoxedABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        use arret_runtime::abitype::BoxedABIType;

        match self {
            BoxedABIType::Any => Ty::Any.into(),
            BoxedABIType::Vector(member) => Ty::Vectorof(Box::new(member.to_ty_ref())).into(),
            BoxedABIType::List(member) => ty::List::new_uniform(member.to_ty_ref()).into(),
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
        use arret_runtime::abitype::BoxedABIType;

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
        use arret_runtime::abitype::RetABIType;

        match self {
            RetABIType::Void => Ty::unit().into(),
            RetABIType::Never => Ty::never().into(),
            RetABIType::Inhabited(abi_type) => abi_type.to_ty_ref(),
        }
    }

    fn to_rust_str(&self) -> String {
        use arret_runtime::abitype::RetABIType;

        match self {
            RetABIType::Void => "()".to_owned(),
            RetABIType::Never => "Never".to_owned(),
            RetABIType::Inhabited(abi_type) => abi_type.to_rust_str(),
        }
    }
}

impl ConvertableABIType for callback::EntryPointABIType {
    fn to_ty_ref<M: ty::PM>(&self) -> ty::Ref<M> {
        // TODO: How do we deal with rest params?
        let fixed_param_ty_refs = self
            .params
            .iter()
            .map(ConvertableABIType::to_ty_ref)
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
        use arret_runtime::abitype::EncodeBoxedABIType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Pair<boxed::Int> as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Pair<boxed::Int>", boxed_abi_type.to_rust_str());

        let int_pair_poly: ty::Ref<ty::Poly> =
            ty::List::new(Box::new([Ty::Int.into()]), Ty::Int.into()).into();

        assert_eq!(int_pair_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn bool_abi_type() {
        use arret_runtime::abitype::EncodeBoxedABIType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Bool as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Bool", boxed_abi_type.to_rust_str());

        let bool_poly: ty::Ref<ty::Poly> = Ty::Bool.into();
        assert_eq!(bool_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn nil_abi_type() {
        use arret_runtime::abitype::EncodeBoxedABIType;
        use arret_runtime::boxed;

        let boxed_abi_type = <boxed::Nil as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Nil", boxed_abi_type.to_rust_str());

        let nil_poly: ty::Ref<ty::Poly> = ty::List::empty().into();
        assert_eq!(nil_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn callback_abi_type() {
        use arret_runtime::task;

        let entry_point_abi_type = <extern "C" fn(&mut task::Task, boxed::Closure, i64) -> char as callback::EncodeEntryPointABIType>::ENTRY_POINT_ABI_TYPE;

        assert_eq!(
            "extern \"C\" fn(&mut Task, boxed::Closure, i64) -> char",
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
}
