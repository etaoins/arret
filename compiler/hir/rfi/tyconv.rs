use ty;

use runtime::{abitype, boxed};

fn type_tag_to_poly_ty(type_tag: boxed::TypeTag) -> ty::Ty<ty::Poly> {
    use runtime::boxed::TypeTag;

    match type_tag {
        TypeTag::Float => ty::Ty::Float,
        TypeTag::Char => ty::Ty::Char,
        TypeTag::Str => ty::Ty::Str,
        TypeTag::Sym => ty::Ty::Sym,
        TypeTag::True => ty::Ty::LitBool(true),
        TypeTag::False => ty::Ty::LitBool(true),
        TypeTag::Int => ty::Ty::Int,
        TypeTag::TopVector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into_poly())),
        TypeTag::Nil => {
            let list = ty::List::new(Box::new([]), None);
            ty::Ty::List(list)
        }
        TypeTag::TopPair => {
            let list = ty::List::new(
                Box::new([ty::Ty::Any.into_poly()]),
                Some(ty::Ty::Any.into_poly()),
            );
            ty::Ty::List(list)
        }
    }
}

pub trait ConvertableABIType {
    fn to_poly(&self) -> ty::Poly;
    fn to_rust_str(&self) -> String;
}

impl ConvertableABIType for abitype::ABIType {
    fn to_poly(&self) -> ty::Poly {
        use runtime::abitype::ABIType;

        match self {
            ABIType::Bool => ty::Ty::Bool.into_poly(),
            ABIType::Char => ty::Ty::Char.into_poly(),
            ABIType::Float => ty::Ty::Float.into_poly(),
            ABIType::Int => ty::Ty::Int.into_poly(),
            ABIType::InternedSym => ty::Ty::Sym.into_poly(),
            ABIType::Boxed(boxed) => boxed.to_poly(),
        }
    }

    fn to_rust_str(&self) -> String {
        use runtime::abitype::ABIType;

        match self {
            ABIType::Bool => "bool".to_owned(),
            ABIType::Char => "char".to_owned(),
            ABIType::Float => "f64".to_owned(),
            ABIType::Int => "i64".to_owned(),
            ABIType::InternedSym => "InternedSym".to_owned(),
            ABIType::Boxed(boxed) => format!("Gc<{}>", boxed.to_rust_str()),
        }
    }
}

impl ConvertableABIType for abitype::BoxedABIType {
    fn to_poly(&self) -> ty::Poly {
        use runtime::abitype::BoxedABIType;

        match self {
            BoxedABIType::Any => ty::Ty::Any.into_poly(),
            BoxedABIType::Vector(member) => {
                ty::Ty::Vectorof(Box::new(member.to_poly())).into_poly()
            }
            BoxedABIType::List(member) => {
                let list = ty::List::new(Box::new([]), Some(member.to_poly()));
                ty::Ty::List(list).into_poly()
            }
            BoxedABIType::Pair(member) => {
                let member_poly = member.to_poly();
                let list = ty::List::new(Box::new([member_poly.clone()]), Some(member_poly));
                ty::Ty::List(list).into_poly()
            }
            BoxedABIType::DirectTagged(type_tag) => type_tag_to_poly_ty(*type_tag).into_poly(),
            BoxedABIType::Union(_, tags) => {
                let members = tags
                    .iter()
                    .map(|type_tag| type_tag_to_poly_ty(*type_tag).into_poly());

                ty::unify::poly_unify_iter(&[], members)
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
    fn to_poly(&self) -> ty::Poly {
        use runtime::abitype::RetABIType;

        match self {
            RetABIType::Inhabited(abi_type) => abi_type.to_poly(),
            RetABIType::Void => ty::Ty::List(ty::List::new(Box::new([]), None)).into_poly(),
        }
    }

    fn to_rust_str(&self) -> String {
        use runtime::abitype::RetABIType;

        match self {
            RetABIType::Inhabited(abi_type) => abi_type.to_rust_str(),
            RetABIType::Void => "()".to_owned(),
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

        assert_eq!(top_list_poly, boxed_abi_type.to_poly());
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

        assert_eq!(int_pair_poly, boxed_abi_type.to_poly());
    }

    #[test]
    fn nil_abi_type() {
        use runtime::abitype::EncodeBoxedABIType;
        use runtime::boxed;

        let boxed_abi_type = <boxed::Nil as EncodeBoxedABIType>::BOXED_ABI_TYPE;

        assert_eq!("boxed::Nil", boxed_abi_type.to_rust_str());

        let nil_poly = ty::Ty::List(ty::List::new(Box::new([]), None)).into_poly();
        assert_eq!(nil_poly, boxed_abi_type.to_poly());
    }
}
