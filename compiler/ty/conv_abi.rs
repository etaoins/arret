use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::unify::Unifiable;

use runtime::{abitype, boxed};

fn type_tag_to_ty<S: Unifiable>(type_tag: boxed::TypeTag) -> ty::Ty<S> {
    use runtime::boxed::TypeTag;

    match type_tag {
        TypeTag::Float => ty::Ty::Float,
        TypeTag::Char => ty::Ty::Char,
        TypeTag::Str => ty::Ty::Str,
        TypeTag::Sym => ty::Ty::Sym,
        TypeTag::True => ty::Ty::LitBool(true),
        TypeTag::False => ty::Ty::LitBool(true),
        TypeTag::Int => ty::Ty::Int,
        TypeTag::TopVector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into_ty_ref())),
        TypeTag::Nil => ty::Ty::List(ty::List::empty()),
        TypeTag::TopPair => {
            let list = ty::List::new(
                Box::new([ty::Ty::Any.into_ty_ref()]),
                Some(ty::Ty::Any.into_ty_ref()),
            );
            ty::Ty::List(list)
        }
        TypeTag::FunThunk => {
            ty::TopFun::new(Purity::Impure.into_poly(), ty::Ty::Any.into_poly()).into_ty()
        }
    }
}

pub trait ConvertableABIType {
    fn to_ty_ref<S: Unifiable>(&self) -> S;
    fn to_rust_str(&self) -> String;
}

impl ConvertableABIType for abitype::ABIType {
    fn to_ty_ref<S: Unifiable>(&self) -> S {
        use runtime::abitype::ABIType;

        match self {
            ABIType::Bool => ty::Ty::Bool.into_ty_ref(),
            ABIType::Char => ty::Ty::Char.into_ty_ref(),
            ABIType::Float => ty::Ty::Float.into_ty_ref(),
            ABIType::Int => ty::Ty::Int.into_ty_ref(),
            ABIType::Boxed(boxed) => boxed.to_ty_ref(),
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
        }
    }
}

impl ConvertableABIType for abitype::BoxedABIType {
    fn to_ty_ref<S: Unifiable>(&self) -> S {
        use runtime::abitype::BoxedABIType;

        match self {
            BoxedABIType::Any => ty::Ty::Any.into_ty_ref(),
            BoxedABIType::Vector(member) => {
                ty::Ty::Vectorof(Box::new(member.to_ty_ref())).into_ty_ref()
            }
            BoxedABIType::List(member) => {
                let list = ty::List::new(Box::new([]), Some(member.to_ty_ref()));
                ty::Ty::List(list).into_ty_ref()
            }
            BoxedABIType::Pair(member) => {
                let member_ty_ref: S = member.to_ty_ref();
                let list = ty::List::new(Box::new([member_ty_ref.clone()]), Some(member_ty_ref));
                ty::Ty::List(list).into_ty_ref()
            }
            BoxedABIType::DirectTagged(type_tag) => type_tag_to_ty(*type_tag).into_ty_ref(),
            BoxedABIType::Union(_, tags) => {
                let members = tags
                    .iter()
                    .map(|type_tag| type_tag_to_ty(*type_tag).into_ty_ref());

                ty::unify::unify_ty_ref_iter(&ty::TVars::new(), members)
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
            BoxedABIType::DirectTagged(type_tag) => format!("boxed::{}", type_tag.to_str()),
            BoxedABIType::Union(name, _) => format!("boxed::{}", name),
        }
    }
}

impl ConvertableABIType for abitype::RetABIType {
    fn to_ty_ref<S: Unifiable>(&self) -> S {
        use runtime::abitype::RetABIType;

        match self {
            RetABIType::Void => ty::Ty::unit().into_ty_ref(),
            RetABIType::Never => ty::Ty::never().into_ty_ref(),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn top_list_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        // `TopList` is actually a union type in the ABI
        let boxed_abi_type = <boxed::TopList as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::TopList", boxed_abi_type.to_rust_str());

        let top_list_poly =
            ty::Ty::List(ty::List::new(Box::new([]), Some(ty::Ty::Any.into_poly()))).into_poly();

        assert_eq!(top_list_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn pair_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Pair<boxed::Int> as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Pair<boxed::Int>", boxed_abi_type.to_rust_str());

        let int_pair_poly = ty::Ty::List(ty::List::new(
            Box::new([ty::Ty::Int.into_poly()]),
            Some(ty::Ty::Int.into_poly()),
        )).into_poly();

        assert_eq!(int_pair_poly, boxed_abi_type.to_ty_ref());
    }

    #[test]
    fn nil_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Nil as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Nil", boxed_abi_type.to_rust_str());

        let nil_poly = ty::Ty::List(ty::List::empty()).into_poly();
        assert_eq!(nil_poly, boxed_abi_type.to_ty_ref());
    }
}
