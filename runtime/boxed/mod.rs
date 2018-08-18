mod heap;
pub mod refs;
mod types;

use std::ptr;

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::intern::Interner;

pub use crate::boxed::heap::Heap;
pub use crate::boxed::types::char::Char;
pub use crate::boxed::types::float::Float;
pub use crate::boxed::types::int::Int;
pub use crate::boxed::types::list::{List, Pair, TopPair};
pub use crate::boxed::types::str::Str;
pub use crate::boxed::types::sym::Sym;
pub use crate::boxed::types::vector::{TopVector, Vector};

pub mod prelude {
    pub use super::AsHeap;
    pub use super::ConstructableFrom;
    pub use super::Downcastable;
}

pub trait Boxed: Sized {}

#[derive(Copy, Clone)]
pub enum BoxSize {
    Size16,
    Size32,
}

impl BoxSize {
    fn cell_count(self) -> usize {
        match self {
            BoxSize::Size16 => 1,
            BoxSize::Size32 => 2,
        }
    }

    fn to_heap_alloc_type(self) -> AllocType {
        match self {
            BoxSize::Size16 => AllocType::Heap16,
            BoxSize::Size32 => AllocType::Heap32,
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq)]
pub enum AllocType {
    Const,
    Stack,
    Heap16,
    Heap32,
    HeapForward16,
    HeapForward32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Header {
    type_tag: TypeTag,
    alloc_type: AllocType,
}

#[repr(C, align(16))]
pub struct Any {
    header: Header,
}

impl Any {
    pub fn downcast_ref<T: Downcastable>(&self) -> Option<Gc<T>> {
        if T::has_tag(self.header.type_tag) {
            Some(unsafe { Gc::new(&*(self as *const Any as *const T)) })
        } else {
            None
        }
    }
}

impl Boxed for Any {}

impl EncodeBoxedABIType for Any {
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Any;
}

pub trait Downcastable: Boxed {
    fn has_tag(type_tag: TypeTag) -> bool;

    fn as_any_ref(&self) -> Gc<Any> {
        unsafe { Gc::new(&*(self as *const Self as *const Any)) }
    }
}

pub trait DirectTagged: Boxed {
    const TYPE_TAG: TypeTag;
}

impl<T: DirectTagged> EncodeBoxedABIType for T {
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::DirectTagged(T::TYPE_TAG);
}

impl<T: DirectTagged> Downcastable for T {
    fn has_tag(type_tag: TypeTag) -> bool {
        Self::TYPE_TAG == type_tag
    }
}

pub trait AsHeap {
    fn as_heap(&self) -> &Heap;
    fn as_heap_mut(&mut self) -> &mut Heap;
}

impl AsHeap for Heap {
    fn as_heap(&self) -> &Heap {
        self
    }

    fn as_heap_mut(&mut self) -> &mut Heap {
        self
    }
}

pub trait ConstructableFrom<T>: Boxed {
    /// Returns the size of the box required to hold the specific value
    ///
    /// This is used to more tightly pack boxes on the heap. It is always safe to return a larger
    /// value than required.
    fn size_for_value(value: &T) -> BoxSize;

    /// Creates a new instance for the given value and box header
    fn construct(value: T, alloc_type: AllocType, interner: &mut Interner) -> Self;

    fn new(heap: &mut impl AsHeap, value: T) -> Gc<Self> {
        heap.as_heap_mut().new_box::<Self, T>(value)
    }
}

macro_rules! define_direct_tagged_boxes {
    ($($name:ident),*) => {
        #[repr(u8)]
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        pub enum TypeTag {
            $( $name ),*
        }

        impl TypeTag {
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

        pub enum AnySubtype<'a> {
            $( $name(&'a $name) ),*
        }

        $(
            impl Boxed for $name {}

            impl DirectTagged for $name {
                const TYPE_TAG: TypeTag = TypeTag::$name;
            }
        )*

        impl Any {
            pub fn as_subtype(&self) -> AnySubtype {
                match self.header.type_tag {
                    $(
                        TypeTag::$name => {
                            AnySubtype::$name(unsafe {
                                &*(self as *const Any as *const $name)
                            })
                        }
                    )*
                }
            }
        }

        impl Drop for Any {
            fn drop(&mut self) {
                // Cast to the correct type so Rust knows which Drop implementation to call
                match self.header.type_tag {
                    $(
                        TypeTag::$name => {
                            unsafe {
                                ptr::drop_in_place(self as *mut Any as *mut $name);
                            }
                        }
                    )*
                }

            }
        }
    }
}

macro_rules! define_singleton_box {
    ($type_name:ident, $static_name:ident) => {
        #[repr(C, align(16))]
        pub struct $type_name {
            header: Header,
        }

        pub static $static_name: $type_name = $type_name {
            header: Header {
                type_tag: $type_name::TYPE_TAG,
                alloc_type: AllocType::Const,
            },
        };
    };
}

macro_rules! define_tagged_union {
    ($name:ident, $subtype_enum:ident, $member_trait:ident, $as_enum_ref:ident, { $($member:ident),* }) => {
        #[repr(C, align(16))]
        pub struct $name {
            header: Header,
        }

        impl $name {
            pub fn downcast_ref<T: $member_trait>(&self) -> Option<Gc<T>> {
                if T::has_tag(self.header.type_tag) {
                    Some(unsafe { Gc::new(&*(self as *const $name as *const T)) })
                } else {
                    None
                }
            }

            pub fn as_subtype(&self) -> $subtype_enum {
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
        }

        impl Boxed for $name {}

        impl EncodeBoxedABIType for $name {
            const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Union(stringify!($name), &[
                $( $member::TYPE_TAG ),*
            ]);
        }

        pub trait $member_trait : Downcastable {}

        impl Downcastable for $name {
            fn has_tag(type_tag: TypeTag) -> bool {
                [$( TypeTag::$member ),*].contains(&type_tag)
            }
        }

        $(
            impl $member_trait for $member {}

            impl $member {
                pub fn $as_enum_ref(&self) -> Gc<$name> {
                    unsafe { Gc::new(&*(self as *const Self as *const $name)) }
                }
            }
        )*

        pub enum $subtype_enum<'a> {
            $( $member(&'a $member) ),*
        }
    };
}

define_direct_tagged_boxes! {
    Float,
    Int,
    Char,
    Str,
    Sym,
    TopPair,
    Nil,
    True,
    False,
    TopVector
}

define_singleton_box!(Nil, NIL_INSTANCE);
define_singleton_box!(True, TRUE_INSTANCE);
define_singleton_box!(False, FALSE_INSTANCE);

define_tagged_union!(Num, NumSubtype, NumMember, as_num_ref, {
    Int,
    Float
});

define_tagged_union!(Bool, BoolSubtype, BoolMember, as_bool_ref, { True, False });

define_tagged_union!(TopList, TopListSubtype, TopListMember, as_top_list_ref, {
    TopPair,
    Nil
});

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(2, mem::size_of::<Header>());
    }

    #[test]
    fn downcast_ref() {
        let mut heap = Heap::new();

        let box_float = Float::new(&mut heap, 2.0);
        let box_float_as_any = box_float.as_any_ref();

        assert_eq!(false, box_float_as_any.downcast_ref::<Int>().is_some());
        assert_eq!(true, box_float_as_any.downcast_ref::<Float>().is_some());
    }

    #[test]
    fn as_tagged() {
        let mut heap = Heap::new();

        let box_float = Float::new(&mut heap, 2.0);
        let box_float_as_any = box_float.as_any_ref();

        if let AnySubtype::Float(_) = box_float_as_any.as_subtype() {
        } else {
            panic!("Failed to get tagged representation")
        }
    }

    #[test]
    fn union_types() {
        let mut heap = Heap::new();

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
