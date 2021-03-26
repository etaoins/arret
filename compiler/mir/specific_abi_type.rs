use arret_runtime::abitype;
use arret_runtime::boxed::TypeTag;

use crate::mir::tagset::TypeTagSet;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::Ty;

const ANY_BOXED_ABI_TYPE: abitype::BoxedAbiType = abitype::BoxedAbiType::Any;

const TOP_RECORD_BOXED_ABI_TYPE: abitype::BoxedAbiType =
    abitype::BoxedAbiType::UniqueTagged(TypeTag::Record);

fn specific_boxed_abi_type_for_type_tag(type_tag: TypeTag) -> &'static abitype::BoxedAbiType {
    use arret_runtime::abitype::EncodeBoxedAbiType;
    use arret_runtime::boxed;

    match type_tag {
        TypeTag::Pair => &boxed::Pair::<boxed::Any>::BOXED_ABI_TYPE,
        TypeTag::Vector => &boxed::Vector::<boxed::Any>::BOXED_ABI_TYPE,
        TypeTag::Char => &boxed::Char::BOXED_ABI_TYPE,
        TypeTag::Int => &boxed::Int::BOXED_ABI_TYPE,
        TypeTag::Float => &boxed::Float::BOXED_ABI_TYPE,
        TypeTag::Str => &boxed::Str::BOXED_ABI_TYPE,
        TypeTag::Sym => &boxed::Sym::BOXED_ABI_TYPE,
        TypeTag::True => &boxed::True::BOXED_ABI_TYPE,
        TypeTag::False => &boxed::False::BOXED_ABI_TYPE,
        TypeTag::Nil => &boxed::Nil::BOXED_ABI_TYPE,
        TypeTag::FunThunk => &boxed::FunThunk::BOXED_ABI_TYPE,
        TypeTag::Record => &TOP_RECORD_BOXED_ABI_TYPE,
        TypeTag::Set => &boxed::Set::<boxed::Any>::BOXED_ABI_TYPE,
        TypeTag::Map => &boxed::Map::<boxed::Any, boxed::Any>::BOXED_ABI_TYPE,
    }
}

fn specific_abi_type_for_type_tag(type_tag: TypeTag) -> abitype::AbiType {
    match type_tag {
        TypeTag::Int => abitype::AbiType::Int,
        TypeTag::Float => abitype::AbiType::Float,
        TypeTag::Char => abitype::AbiType::Char,
        TypeTag::Sym => abitype::AbiType::InternedSym,
        other_tag => specific_boxed_abi_type_for_type_tag(other_tag)
            .clone()
            .into(),
    }
}

fn specific_boxed_abi_type_for_type_tags(
    possible_type_tags: TypeTagSet,
) -> &'static abitype::BoxedAbiType {
    use arret_runtime::abitype::EncodeBoxedAbiType;
    use arret_runtime::boxed;

    if possible_type_tags.len() == 1 {
        let single_type_tag = possible_type_tags.into_iter().next().unwrap();
        specific_boxed_abi_type_for_type_tag(single_type_tag)
    } else if possible_type_tags == [TypeTag::Pair, TypeTag::Nil].iter().collect() {
        &boxed::List::<boxed::Any>::BOXED_ABI_TYPE
    } else if possible_type_tags == [TypeTag::Float, TypeTag::Int].iter().collect() {
        &boxed::Num::BOXED_ABI_TYPE
    } else if possible_type_tags == [TypeTag::True, TypeTag::False].iter().collect() {
        &boxed::Bool::BOXED_ABI_TYPE
    } else {
        &ANY_BOXED_ABI_TYPE
    }
}

pub fn specific_boxed_abi_type_for_ty_ref<M: ty::Pm>(
    ty_ref: &ty::Ref<M>,
) -> &'static abitype::BoxedAbiType {
    specific_boxed_abi_type_for_type_tags(ty_ref.into())
}

fn specific_abi_type_for_type_tags(possible_type_tags: TypeTagSet) -> abitype::AbiType {
    if possible_type_tags.is_subset([TypeTag::True, TypeTag::False].iter().collect()) {
        abitype::AbiType::Bool
    } else if possible_type_tags.len() == 1 {
        let single_type_tag = possible_type_tags.into_iter().next().unwrap();
        specific_abi_type_for_type_tag(single_type_tag)
    } else {
        specific_boxed_abi_type_for_type_tags(possible_type_tags)
            .clone()
            .into()
    }
}

/// Returns a specific ABI type to encode the given ty_ref
pub fn specific_abi_type_for_ty_ref<M: ty::Pm>(ty_ref: &ty::Ref<M>) -> abitype::AbiType {
    use crate::ty::list_iter::ListIterator;

    match ty_ref.resolve_to_ty() {
        Ty::List(list_ty) if !list_ty.is_empty() => {
            let member_ty_ref = ListIterator::new(list_ty).collect_rest();
            let member_boxed_abi_type = specific_boxed_abi_type_for_ty_ref(&member_ty_ref);

            if list_ty.fixed().is_empty() {
                abitype::BoxedAbiType::List(member_boxed_abi_type).into()
            } else {
                abitype::BoxedAbiType::Pair(member_boxed_abi_type).into()
            }
        }
        Ty::Vectorof(member_ty) => {
            let member_boxed_abi_type = specific_boxed_abi_type_for_ty_ref(member_ty.as_ref());
            abitype::BoxedAbiType::Vector(member_boxed_abi_type).into()
        }
        Ty::Vector(member_tys) => {
            let member_ty_ref = ty::unify::unify_ty_ref_iter(member_tys.iter().cloned());
            let member_boxed_abi_type = specific_boxed_abi_type_for_ty_ref(&member_ty_ref);

            abitype::BoxedAbiType::Vector(member_boxed_abi_type).into()
        }
        _ => specific_abi_type_for_type_tags(ty_ref.into()),
    }
}

pub fn specific_ret_abi_type_for_ty_ref<M: ty::Pm>(ty_ref: &ty::Ref<M>) -> abitype::RetAbiType {
    if ty_ref == &ty::List::empty().into() {
        abitype::RetAbiType::Void
    } else {
        specific_abi_type_for_type_tags(ty_ref.into()).into()
    }
}

fn specific_type_for_values<'v, F, T>(
    possible_values: impl Iterator<Item = &'v Value>,
    tagset_to_type: F,
) -> T
where
    F: FnOnce(TypeTagSet) -> T,
{
    use crate::mir::value::types::possible_type_tags_for_value;

    let possible_type_tags = possible_values
        .map(possible_type_tags_for_value)
        .fold(TypeTagSet::new(), |acc, type_tags| acc | type_tags);

    tagset_to_type(possible_type_tags)
}

/// Returns a specific boxed ABI type to encode the given set of possible values
pub fn specific_boxed_abi_type_for_values<'v>(
    possible_values: impl Iterator<Item = &'v Value>,
) -> abitype::BoxedAbiType {
    specific_type_for_values(possible_values, specific_boxed_abi_type_for_type_tags).clone()
}

/// Returns a specific ABI type to compactly encode the given set of possible values
pub fn specific_abi_type_for_values<'v>(
    possible_values: impl Iterator<Item = &'v Value>,
) -> abitype::AbiType {
    specific_type_for_values(possible_values, specific_abi_type_for_type_tags)
}

/// Return a specific ABI type to compactly encode the given value
pub fn specific_abi_type_for_value(value: &Value) -> abitype::AbiType {
    specific_abi_type_for_values(std::iter::once(value))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;
    use arret_runtime::abitype::EncodeBoxedAbiType;
    use arret_runtime::boxed;

    fn assert_abi_type_for_str(abi_type: abitype::AbiType, ty_str: &'static str) {
        let poly = poly_for_str(ty_str);
        assert_eq!(abi_type, specific_abi_type_for_ty_ref(&poly));
    }

    #[test]
    fn test_specific_abi_type_for_ty_ref() {
        assert_abi_type_for_str(abitype::AbiType::Bool, "true");
        assert_abi_type_for_str(abitype::AbiType::Bool, "false");
        assert_abi_type_for_str(abitype::AbiType::Bool, "Bool");

        assert_abi_type_for_str(abitype::AbiType::Float, "Float");
        assert_abi_type_for_str(abitype::AbiType::Int, "Int");
        assert_abi_type_for_str(boxed::Num::BOXED_ABI_TYPE.into(), "Num");
        assert_abi_type_for_str(abitype::AbiType::Char, "Char");
        assert_abi_type_for_str(abitype::AbiType::InternedSym, "Sym");
        assert_abi_type_for_str(abitype::BoxedAbiType::Any.into(), "(RawU Num Bool)");

        assert_abi_type_for_str(boxed::Nil::BOXED_ABI_TYPE.into(), "(List)");

        assert_abi_type_for_str(
            abitype::BoxedAbiType::List(&boxed::Bool::BOXED_ABI_TYPE).into(),
            "(List & Bool)",
        );

        assert_abi_type_for_str(
            abitype::BoxedAbiType::Pair(&boxed::Num::BOXED_ABI_TYPE).into(),
            "(List Float & Int)",
        );

        assert_abi_type_for_str(
            abitype::BoxedAbiType::Vector(&boxed::Str::BOXED_ABI_TYPE).into(),
            "(Vectorof Str)",
        );

        assert_abi_type_for_str(
            abitype::BoxedAbiType::Vector(&boxed::Sym::BOXED_ABI_TYPE).into(),
            "(Vector 'foo 'bar)",
        );
    }
}
