#![warn(missing_docs)]

//! Boxed values and heaps
//!
//! This contains the implementation of our garbage collector and the types it can manage. Some
//! types (such as `Int` and `Float`) have corresponding unboxed representations and are only boxed
//! for the purposes of runtime dynamic typing. Complex values (such as `Vector` and `Sym`) have no
//! unboxed representation.
//!
//! Boxes can also be placed on the stack on in static constants instead of the heap. This is of
//! limited value to Rust code but is frequently used by the compiler to avoid the overhead of
//! allocation and garbage collection.

mod heap;
pub mod refs;
mod types;

use std::hash::{Hash, Hasher};
use std::{fmt, ptr};

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;

pub use crate::boxed::heap::{collect, type_info};
pub use crate::boxed::heap::{AsHeap, Heap};
pub use crate::boxed::types::char::Char;
pub use crate::boxed::types::float::Float;
pub use crate::boxed::types::fun::{Closure, FunThunk, ThunkEntry};
pub use crate::boxed::types::int::Int;
pub use crate::boxed::types::list::{List, Nil, Pair, NIL_INSTANCE};
pub use crate::boxed::types::record::{Record, RecordClassId, RecordStorage};
pub use crate::boxed::types::str::Str;
pub use crate::boxed::types::sym::Sym;
pub use crate::boxed::types::vector::Vector;

/// Prelude of common traits useful for working with boxed values
pub mod prelude {
    pub use super::AsHeap;
    pub use super::Boxed;
    pub use super::DistinctTagged;
    pub use super::HashInHeap;
    pub use super::PartialEqInHeap;
}

/// Size of a boxed value in bytes
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BoxSize {
    /// 16 byte boxed value
    Size16,
    /// 32 byte boxed value
    Size32,
}

impl BoxSize {
    /// Returns the number of 16 byte cells required by this box size
    pub fn cell_count(self) -> usize {
        match self {
            BoxSize::Size16 => 1,
            BoxSize::Size32 => 2,
        }
    }

    /// Returns the corresponding `AllocType` if this box was allocated on the heap
    pub fn to_heap_alloc_type(self) -> AllocType {
        match self {
            BoxSize::Size16 => AllocType::Heap16,
            BoxSize::Size32 => AllocType::Heap32,
        }
    }
}

/// Allocation type for boxed values
#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AllocType {
    /// Static constant value
    Const,
    /// Stack allocated value of unknown length
    Stack,
    /// Heap allocated 16 byte value
    Heap16,
    /// Heap allocated 32 byte value
    Heap32,

    /// Box pointing to a new 16 byte heap location
    ///
    /// This is a temporary type used during garbage collection.
    HeapForward16,

    /// Box pointing to a new 32 byte heap location
    ///
    /// This is a temporary type used during garbage collection.
    HeapForward32,
}

impl AllocType {
    /// Returns the corresponding `BoxSize` if this type is heap allocated
    pub fn to_heap_box_size(self) -> Option<BoxSize> {
        match self {
            AllocType::Heap16 => Some(BoxSize::Size16),
            AllocType::Heap32 => Some(BoxSize::Size32),
            _ => None,
        }
    }
}

/// Header for common boxed value metadata
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Header {
    type_tag: TypeTag,
    alloc_type: AllocType,
}

impl Header {
    /// Returns a new header for the given type tag and allocation type
    pub fn new(type_tag: TypeTag, alloc_type: AllocType) -> Header {
        Header {
            type_tag,
            alloc_type,
        }
    }

    /// Returns the constant type tag for this value
    pub fn type_tag(self) -> TypeTag {
        self.type_tag
    }

    /// Return the allocation type for this value
    pub fn alloc_type(self) -> AllocType {
        self.alloc_type
    }
}

/// Equivalent of [`PartialEq`] that receives an additional [`Heap`] parameter
///
/// This is required for types that require additional metadata from the heap to perform equality
/// checks.
pub trait PartialEqInHeap {
    /// Returns true if the values are equal
    ///
    /// Both values will be in the same heap.
    fn eq_in_heap(&self, heap: &Heap, other: &Self) -> bool;
}

impl<T> PartialEqInHeap for T
where
    T: PartialEq,
{
    fn eq_in_heap(&self, _heap: &Heap, other: &Self) -> bool {
        self.eq(other)
    }
}

/// Equivalent of [`Hash`] that receives an additional [`Heap`] parameter
///
/// This is required for types that require additional metadata from the heap to calculate hashes.
pub trait HashInHeap {
    /// Feeds this value into the given [`Hasher`]
    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H);
}

impl<T> HashInHeap for T
where
    T: Hash,
{
    fn hash_in_heap<H: Hasher>(&self, _heap: &Heap, state: &mut H) {
        self.hash(state)
    }
}

/// Boxed value
///
/// Boxes can be allocated on the stack, heap or a static constant. Every box is tagged with a
/// top-level type.
pub trait Boxed: Sized + PartialEqInHeap + HashInHeap + fmt::Debug {
    /// Casts this value to an `Any` reference
    fn as_any_ref(&self) -> Gc<Any> {
        unsafe { Gc::new(&*(self as *const Self as *const Any)) }
    }

    /// Returns the header of the box
    fn header(&self) -> Header {
        self.as_any_ref().header
    }
}

impl EncodeBoxedABIType for Any {
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Any;
}

/// Marks that this boxed struct has a specific constant type tag
///
/// For example, [`Vector<Str>`] is `ConstTagged` because it always has a type tag of `Vector`. As
/// a counterexample, [`Num`] is not because it could either have an `Int` or `Float` type tag.
///
/// In mathematical terms this can be thought of as the struct being surjective to the type tag.
pub trait ConstTagged: Boxed {
    /// Type tag for values of this type
    const TYPE_TAG: TypeTag;
}

/// Indicates that this boxed struct does not share type tags with unrelated types
///
/// For example, [`Num`] is `DistinctTagged` because it only shares type tags with `Any`, `Float`
/// and `Int` which are all either subtypes or supertypes. As a counterexample, [`Vector<Str>`] is
/// not because it shares a type tag with [`Vector<Sym>`].
///
/// In mathematical terms this can be thought of as the struct being injective to the type tag
pub trait DistinctTagged: Boxed {
    /// Returns if the passed type tag corresponds to this type
    fn has_tag(type_tag: TypeTag) -> bool;
}

/// Marks that every boxed value with `TYPE_TAG` corresponds to this boxed struct
///
/// For example, [`Str`] is `UniqueTagged` because no other struct has the type tag of `Str`. As a
/// counterexample, `Vector<Str>` is not because it shares a type tag with `Vector<Sym>`.
///
/// In mathematical terms this can be thought of as the struct being bijective with the type tag.
pub trait UniqueTagged: ConstTagged + DistinctTagged {}

impl<T: UniqueTagged> EncodeBoxedABIType for T {
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::UniqueTagged(T::TYPE_TAG);
}

macro_rules! define_const_tagged_boxes {
    ($($name:ident),*) => {
        /// Tag byte identifying top-level types
        #[repr(u8)]
        #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
        pub enum TypeTag {
            $(
                #[allow(missing_docs)]
                $name
            ),*
        }

        /// Static list of all possible type tags
        ///
        /// This is guaranteed to be sorted
        pub const ALL_TYPE_TAGS: &'static [TypeTag] = &[
            $( TypeTag::$name ),*
        ];

        impl TypeTag {
            /// Returns a string representation for the type
            pub fn to_str(&self) -> &'static str {
                match self {
                    $(
                        TypeTag::$name => {
                            stringify!($name)
                        }
                    )*
                }
            }
        }

        $(
            impl ConstTagged for $name {
                const TYPE_TAG: TypeTag = TypeTag::$name;
            }

            impl DistinctTagged for $name {
                fn has_tag(type_tag: TypeTag) -> bool {
                    Self::TYPE_TAG == type_tag
                }
            }
        )*

        define_supertype!(
            /// Supertype of all boxed types
            Any,
            AnySubtype, DistinctTagged, as_any_ref, { $($name),* });
    }
}

impl TypeTag {
    /// Returns the boxed ABI type corresponding to this type tag
    pub fn to_boxed_abi_type(self) -> BoxedABIType {
        BoxedABIType::UniqueTagged(self)
    }

    /// Returns a header for a constant boxed values of this type
    pub fn to_const_header(self) -> Header {
        Header::new(self, AllocType::Const)
    }

    /// Returns a header for heap allocated values of this type and size
    pub fn to_heap_header(self, box_size: BoxSize) -> Header {
        Header::new(self, box_size.to_heap_alloc_type())
    }
}

macro_rules! define_singleton_box {
    (
        $(#[$struct_docs:meta])*
        $type_name:ident,
        $(#[$static_docs:meta])*
        $static_name:ident,
        $export_name:expr
    ) => {
        $(#[$struct_docs])*
        #[repr(C, align(16))]
        #[derive(Debug)]
        pub struct $type_name {
            header: Header,
        }

        impl Boxed for $type_name {}
        impl UniqueTagged for $type_name {}

        $(#[$static_docs])*
        #[export_name = $export_name]
        pub static $static_name: $type_name = $type_name {
            header: Header {
                type_tag: $type_name::TYPE_TAG,
                alloc_type: AllocType::Const,
            },
        };

        impl PartialEq for $type_name {
            fn eq(&self, _: &$type_name) -> bool {
                // This is tricky - we're a singleton so if the types match we must be equal
                true
            }
        }

        impl Hash for $type_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                Self::TYPE_TAG.hash(state);
                state.write_usize(&$static_name as *const $type_name as usize);
            }
        }
    };
}

macro_rules! define_supertype {
    (
        $(#[$docs:meta])*
        $name:ident,
        $subtype_enum:ident, $subtype_trait:ident, $as_enum_ref:ident, { $($member:ident),* }
    ) => {
        $(#[$docs])*
        #[repr(C, align(16))]
        pub struct $name {
            header: Header,
        }

        impl Boxed for $name {}

        impl DistinctTagged for $name {
            fn has_tag(type_tag: TypeTag) -> bool {
                [$( TypeTag::$member ),*].contains(&type_tag)
            }
        }

        impl $name {
            /// Returns a subtype of this value based on its type tag
            pub fn as_subtype(&self) -> $subtype_enum<'_> {
                #[allow(unreachable_patterns)]
                match self.header.type_tag {
                    $(
                        TypeTag::$member => {
                            $subtype_enum::$member(unsafe {
                                &*(self as *const $name as *const $member)
                            })
                        }
                    )*
                    other => {
                        unreachable!("Unexpected type tag: {:?}", other);
                    }
                }
            }

            /// Tries to downcast this reference to a subtype based on its type tag
            pub fn downcast_ref<T: $subtype_trait>(&self) -> Option<Gc<T>>
            {
                if T::has_tag(self.header.type_tag) {
                    Some(unsafe { Gc::new(&*(self as *const $name as *const T)) })
                } else {
                    None
                }
            }
        }

        impl HashInHeap for $name {
            fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
                match self.as_subtype() {
                    $(
                        $subtype_enum::$member(subtype) => {
                            subtype.hash_in_heap(heap, state)
                        }
                    )*
                }
            }
        }

        impl PartialEqInHeap for $name {
            fn eq_in_heap(&self, heap: &Heap, other: &$name) -> bool {
                match (self.as_subtype(), other.as_subtype()) {
                    $(
                        ($subtype_enum::$member(self_value), $subtype_enum::$member(other_value)) => {
                            self_value.eq_in_heap(heap, other_value)
                        }
                    ),*
                    _ => false
                }
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
                match self.as_subtype() {
                    $(
                        $subtype_enum::$member(subtype) => {
                            subtype.fmt(formatter)
                        }
                    )*
                }
            }
        }

        impl Drop for $name {
            fn drop(&mut self) {
                // Cast to the correct type so Rust knows which Drop implementation to call
                match self.as_subtype() {
                    $(
                        $subtype_enum::$member(subtype) => {
                            unsafe {
                                ptr::drop_in_place(subtype as *const $member as *mut $member);
                            }
                        }
                    )*
                }
            }
        }

        /// Possible subtypes of this supertype
        #[derive(Debug)]
        pub enum $subtype_enum<'a> {
            $(
                #[allow(missing_docs)]
                $member(&'a $member)
            ),*
        }
    }
}

macro_rules! define_tagged_union {
    (
        $(#[$struct_docs:meta])*
        $name:ident,
        $(#[$subtype_docs:meta])*
        $subtype_enum:ident,
        $subtype_trait:ident, $as_enum_ref:ident, { $($member:ident),* }
    ) => {
        define_supertype!(
            $(#[$struct_docs])*
            $name,
            $subtype_enum, $subtype_trait, $as_enum_ref, { $($member),* }
        );

        $(#[$subtype_docs])*
        pub trait $subtype_trait : DistinctTagged {}

        $(
            impl $member {
                /// Casts this value to its supertype
                pub fn $as_enum_ref(&self) -> Gc<$name> {
                    unsafe { Gc::new(&*(self as *const Self as *const $name)) }
                }
            }

            impl $subtype_trait for $member {}
        )*

        impl EncodeBoxedABIType for $name {
            const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Union(stringify!($name), &[
                $( $member::TYPE_TAG ),*
            ]);
        }
    };
}

define_const_tagged_boxes! {
    Float,
    Int,
    Char,
    Str,
    Sym,
    Pair,
    Nil,
    True,
    False,
    Vector,
    FunThunk,
    Record
}

define_singleton_box!(
    /// Boolean true
    True,
    /// Static constant instance of [`True`]
    TRUE_INSTANCE,
    "ARRET_TRUE"
);

define_singleton_box!(
    /// Boolean false
    False,
    /// Static constant instance of [`False`]
    FALSE_INSTANCE,
    "ARRET_FALSE"
);

define_tagged_union!(
    /// Union of numeric types
    Num,
    /// Possible subtypes of [`Num`]
    NumSubtype,
    NumMember, as_num_ref, {
        Int,
        Float
    }
);

define_tagged_union!(
    /// Union of boolean types
    Bool,
    /// Possible subtypes of [`Bool`]
    BoolSubtype,
    BoolMember, as_bool_ref, {
        True,
        False
    }
);

impl Bool {
    /// Returns the singleton box corresponding the boolean value
    pub fn singleton_ref(value: bool) -> Gc<Bool> {
        if value {
            TRUE_INSTANCE.as_bool_ref()
        } else {
            FALSE_INSTANCE.as_bool_ref()
        }
    }

    /// Returns the unboxed value of this boolean
    pub fn as_bool(&self) -> bool {
        match self.as_subtype() {
            BoolSubtype::True(_) => true,
            BoolSubtype::False(_) => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(2, mem::size_of::<Header>());
        assert_eq!(16, mem::size_of::<Nil>());
        assert_eq!(16, mem::size_of::<True>());
        assert_eq!(16, mem::size_of::<False>());
    }

    #[test]
    fn downcast_ref() {
        let mut heap = Heap::empty();

        let box_float = Float::new(&mut heap, 2.0);
        let box_float_as_any = box_float.as_any_ref();

        assert_eq!(false, box_float_as_any.downcast_ref::<Int>().is_some());
        assert_eq!(true, box_float_as_any.downcast_ref::<Float>().is_some());
    }

    #[test]
    fn as_tagged() {
        let mut heap = Heap::empty();

        let box_float = Float::new(&mut heap, 2.0);
        let box_float_as_any = box_float.as_any_ref();

        if let AnySubtype::Float(_) = box_float_as_any.as_subtype() {
        } else {
            panic!("Failed to get tagged representation")
        }
    }

    #[test]
    fn any_equality() {
        let mut heap = Heap::empty();

        let box_two = Float::new(&mut heap, 2.0);
        let box_two_as_any = box_two.as_any_ref();

        let box_three = Float::new(&mut heap, 3.0);
        let box_three_as_any = box_three.as_any_ref();

        assert_eq!(true, box_two_as_any.eq_in_heap(&heap, &box_two_as_any));
        assert_eq!(false, box_two_as_any.eq_in_heap(&heap, &box_three_as_any));

        assert_eq!(TRUE_INSTANCE, TRUE_INSTANCE);
    }

    #[test]
    fn any_fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Int::new(&mut heap, 1);
        let boxed_one_as_any = boxed_one.as_any_ref();
        assert_eq!("Int(1)", format!("{:?}", boxed_one_as_any));
    }

    #[test]
    fn union_types() {
        let mut heap = Heap::empty();

        let box_float = Float::new(&mut heap, 2.0);
        let box_float_as_any = box_float.as_any_ref();

        if let Some(stack_num) = box_float_as_any.downcast_ref::<Num>() {
            if let NumSubtype::Float(_) = stack_num.as_subtype() {
            } else {
                panic!("Couldn't get tagged Float from Num");
            }

            assert_eq!(false, stack_num.downcast_ref::<Int>().is_some());
            assert_eq!(true, stack_num.downcast_ref::<Float>().is_some());
        } else {
            panic!("Float was not a Num");
        }

        let box_str = Str::new(&mut heap, "Test!");
        let box_str_as_any = box_str.as_any_ref();

        assert_eq!(false, box_str_as_any.downcast_ref::<Num>().is_some());
    }
}
