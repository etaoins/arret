use std::{iter, ops};

use crate::ty;
use crate::ty::Ty;
use arret_runtime::abitype;
use arret_runtime::boxed::{TypeTag, ALL_TYPE_TAGS};

const INNER_BITS: u8 = 32;
type Inner = u32;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct TypeTagSet(Inner);

/// Efficient representation of a set of TypeTag
impl TypeTagSet {
    pub fn new() -> TypeTagSet {
        TypeTagSet(0)
    }

    pub fn all() -> TypeTagSet {
        ALL_TYPE_TAGS.iter().collect()
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn len(self) -> usize {
        self.0.count_ones() as usize
    }

    pub fn insert(&mut self, type_tag: TypeTag) {
        // The compiler is smart enough to eliminate this
        assert!((type_tag as u8) < INNER_BITS);
        self.0 |= 1 << type_tag as u8;
    }

    pub fn is_subset(self, superset: Self) -> bool {
        (self.0 & superset.0) == self.0
    }

    pub fn is_disjoint(self, other: Self) -> bool {
        self.intersection(other).is_empty()
    }

    pub fn intersection(self, other: Self) -> TypeTagSet {
        TypeTagSet(self.0 & other.0)
    }

    pub fn union(self, other: Self) -> TypeTagSet {
        TypeTagSet(self.0 | other.0)
    }

    pub fn contains(self, type_tag: TypeTag) -> bool {
        let type_tag_set: TypeTagSet = type_tag.into();
        type_tag_set.is_subset(self)
    }

    /// Returns an iterator over all type tags in the set
    ///
    /// These are returned in sorted order.
    pub fn into_iter(self) -> impl Iterator<Item = TypeTag> {
        ALL_TYPE_TAGS
            .iter()
            .cloned()
            .filter(move |type_tag| self.contains(*type_tag))
    }
}

impl From<TypeTag> for TypeTagSet {
    fn from(type_tag: TypeTag) -> TypeTagSet {
        let mut type_tag_set = TypeTagSet::new();
        type_tag_set.insert(type_tag);
        type_tag_set
    }
}

impl<'a, M> From<&'a ty::Ref<M>> for TypeTagSet
where
    M: ty::PM,
{
    fn from(ty_ref: &'a ty::Ref<M>) -> TypeTagSet {
        match ty_ref.resolve_to_ty() {
            Ty::Any => TypeTagSet::all(),
            Ty::Int => TypeTag::Int.into(),
            Ty::Float => TypeTag::Float.into(),
            Ty::Char => TypeTag::Char.into(),
            Ty::Bool => [TypeTag::True, TypeTag::False].iter().collect(),
            Ty::Num => [TypeTag::Int, TypeTag::Float].iter().collect(),
            Ty::LitBool(true) => TypeTag::True.into(),
            Ty::LitBool(false) => TypeTag::False.into(),
            Ty::Sym | Ty::LitSym(_) => TypeTag::Sym.into(),
            Ty::Str => TypeTag::Str.into(),
            Ty::Fun(_) | Ty::TopFun(_) | Ty::TyPred(_) | Ty::EqPred => TypeTag::FunThunk.into(),
            Ty::Vector(_) | Ty::Vectorof(_) => TypeTag::Vector.into(),
            Ty::Set(_) => TypeTag::Set.into(),
            Ty::TopRecord | Ty::RecordClass(_) | Ty::Record(_) => TypeTag::Record.into(),
            Ty::List(list) => {
                if list.is_empty() {
                    TypeTag::Nil.into()
                } else if !list.fixed().is_empty() {
                    TypeTag::Pair.into()
                } else {
                    [TypeTag::Nil, TypeTag::Pair].iter().collect()
                }
            }
            Ty::Union(members) => members
                .iter()
                .map(TypeTagSet::from)
                .fold(TypeTagSet::new(), |a, b| a | b),
            Ty::Intersect(members) => members
                .iter()
                .map(TypeTagSet::from)
                .fold(TypeTagSet::all(), |a, b| a & b),
            Ty::Map(_) => todo!("no corresponding type tag"),
        }
    }
}

impl<'a> From<&'a abitype::BoxedABIType> for TypeTagSet {
    fn from(boxed_abi_type: &'a abitype::BoxedABIType) -> TypeTagSet {
        use arret_runtime::abitype::BoxedABIType;

        match boxed_abi_type {
            BoxedABIType::Any => TypeTagSet::all(),
            BoxedABIType::UniqueTagged(type_tag) => (*type_tag).into(),
            BoxedABIType::List(_) => [TypeTag::Pair, TypeTag::Nil].iter().collect(),
            BoxedABIType::Pair(_) => TypeTag::Pair.into(),
            BoxedABIType::Vector(_) => TypeTag::Vector.into(),
            BoxedABIType::Set(_) => TypeTag::Set.into(),
            BoxedABIType::Map(_, _) => TypeTag::Map.into(),
            BoxedABIType::Union(_, type_tags) => type_tags.iter().collect(),
        }
    }
}

impl<'a> From<&'a abitype::ABIType> for TypeTagSet {
    fn from(abi_type: &'a abitype::ABIType) -> TypeTagSet {
        use arret_runtime::abitype::ABIType;

        match abi_type {
            ABIType::Int => TypeTag::Int.into(),
            ABIType::Float => TypeTag::Float.into(),
            ABIType::Char => TypeTag::Char.into(),
            ABIType::Bool => [TypeTag::True, TypeTag::False].iter().collect(),
            ABIType::InternedSym => TypeTag::Sym.into(),
            ABIType::Boxed(boxed_abi_type) => boxed_abi_type.into(),
            ABIType::Callback(_) => TypeTag::FunThunk.into(),
        }
    }
}

impl From<abitype::ABIType> for TypeTagSet {
    fn from(abi_type: abitype::ABIType) -> TypeTagSet {
        (&abi_type).into()
    }
}

impl iter::FromIterator<TypeTag> for TypeTagSet {
    fn from_iter<I: IntoIterator<Item = TypeTag>>(iter: I) -> TypeTagSet {
        let mut type_tag_set = TypeTagSet::new();
        for type_tag in iter {
            type_tag_set.insert(type_tag);
        }
        type_tag_set
    }
}

impl<'a> iter::FromIterator<&'a TypeTag> for TypeTagSet {
    fn from_iter<I: IntoIterator<Item = &'a TypeTag>>(iter: I) -> TypeTagSet {
        iter.into_iter().cloned().collect()
    }
}

impl ops::BitOr for TypeTagSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        self.union(rhs)
    }
}

impl ops::BitAnd for TypeTagSet {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        self.intersection(rhs)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::source::EMPTY_SPAN;

    #[test]
    fn basic_operations() {
        let empty_set = TypeTagSet::new();
        let list_set: TypeTagSet = [TypeTag::Nil, TypeTag::Pair].iter().cloned().collect();
        let nil_sym_set: TypeTagSet = [TypeTag::Nil, TypeTag::Sym].iter().cloned().collect();
        let pair_set: TypeTagSet = TypeTag::Pair.into();
        let nil_set: TypeTagSet = TypeTag::Nil.into();
        let full_set = TypeTagSet::all();

        assert!(empty_set.is_empty());
        assert!(!full_set.is_empty());
        assert!(!nil_sym_set.is_empty());
        assert!(!pair_set.is_empty());
        assert!(!full_set.is_empty());

        assert!(empty_set.is_subset(full_set));
        assert!(empty_set.is_subset(nil_sym_set));
        assert!(empty_set.is_subset(empty_set));

        assert!(list_set.is_subset(full_set));
        assert!(list_set.is_subset(list_set));
        assert!(!list_set.is_subset(pair_set));
        assert!(!list_set.is_subset(empty_set));

        assert!(empty_set.is_disjoint(full_set));
        assert!(nil_sym_set.is_disjoint(pair_set));
        assert!(!nil_sym_set.is_disjoint(list_set));

        assert_eq!(nil_set, list_set & nil_sym_set);
        assert_eq!(list_set, pair_set | nil_set);
    }

    #[test]
    fn set_into_iter() {
        use std::collections::HashSet;

        let empty_set = TypeTagSet::new();
        let list_set: TypeTagSet = [TypeTag::Nil, TypeTag::Pair].iter().collect();
        let nil_set: TypeTagSet = TypeTag::Nil.into();
        let full_set = TypeTagSet::all();

        assert_eq!(None, empty_set.into_iter().next());

        let mut nil_set_iter = nil_set.into_iter();
        assert_eq!(Some(TypeTag::Nil), nil_set_iter.next());
        assert_eq!(None, nil_set_iter.next());

        let list_hash_set: HashSet<TypeTag> = list_set.into_iter().collect();
        assert_eq!(2, list_hash_set.len());
        assert!(list_hash_set.contains(&TypeTag::Pair));
        assert!(list_hash_set.contains(&TypeTag::Nil));

        assert_eq!(ALL_TYPE_TAGS.len(), full_set.into_iter().count());
    }

    #[test]
    fn from_ty_ref() {
        let int_ty_ref: ty::Ref<ty::Poly> = Ty::Int.into();
        assert_eq!(
            TypeTagSet::from(TypeTag::Int),
            TypeTagSet::from(&int_ty_ref)
        );

        let poly_sym_ref: ty::Ref<ty::Poly> =
            ty::TVar::new(EMPTY_SPAN, "tvar1".into(), Ty::Sym.into()).into();
        assert_eq!(
            TypeTagSet::from(TypeTag::Sym),
            TypeTagSet::from(&poly_sym_ref)
        );

        let num_float_intersect: ty::Ref<ty::Poly> =
            Ty::Intersect(Box::new([Ty::Num.into(), Ty::Float.into()])).into();
        assert_eq!(
            TypeTagSet::from(TypeTag::Float),
            TypeTagSet::from(&num_float_intersect)
        );
    }
}
