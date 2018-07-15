mod gcref;
mod heap;
mod types;

pub use boxed::types::float::Float;
pub use boxed::types::int::Int;
pub use boxed::types::nil::Nil;
pub use boxed::types::pair::Pair;
pub use boxed::types::str::Str;

pub use boxed::heap::Heap;

pub use boxed::gcref::Gc;

#[derive(Copy, Clone)]
pub enum HeapSize {
    Size16,
    Size32,
}

impl HeapSize {
    fn cell_count(self) -> usize {
        match self {
            HeapSize::Size16 => 1,
            HeapSize::Size32 => 2,
        }
    }

    fn to_alloc_type(self) -> AllocType {
        match self {
            HeapSize::Size16 => AllocType::Heap16,
            HeapSize::Size32 => AllocType::Heap32,
        }
    }
}

#[repr(u8)]
#[derive(Debug)]
pub enum AllocType {
    Const,
    Stack,
    Heap16,
    Heap32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Header {
    pub type_tag: TypeTag,
    pub alloc_type: AllocType,
}

#[repr(C, align(16))]
pub struct Any {
    pub header: Header,
}

impl Any {
    pub fn downcast_ref<T>(&self) -> Option<&T>
    where
        T: AnyDowncastable,
    {
        if T::has_tag(self.header.type_tag) {
            Some(unsafe { &*(self as *const Any as *const T) })
        } else {
            None
        }
    }
}

pub trait AnyDowncastable {
    fn has_tag(type_tag: TypeTag) -> bool;
}

pub trait DirectTagged {
    const TYPE_TAG: TypeTag;
}

impl<T> AnyDowncastable for T
where
    T: DirectTagged,
{
    fn has_tag(type_tag: TypeTag) -> bool {
        Self::TYPE_TAG == type_tag
    }
}

pub trait ConstructableFrom<T>: DirectTagged + Sized {
    fn heap_size_for_value(value: &T) -> HeapSize;
    fn new_with_header(value: T, header: Header) -> Self;

    fn new(value: T) -> Self {
        Self::new_with_header(
            value,
            Header {
                type_tag: Self::TYPE_TAG,
                alloc_type: AllocType::Stack,
            },
        )
    }
}

macro_rules! define_tagged_boxes {
    ($($name:ident),*) => {
        #[repr(u8)]
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        pub enum TypeTag {
            $($name),*
        }

        pub enum AnySubtype<'a> {
            $($name(&'a $name)),*
        }

        $(
            impl DirectTagged for $name {
                const TYPE_TAG: TypeTag = TypeTag::$name;
            }
        )*

        impl Any {
            fn as_subtype(&self) -> AnySubtype {
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
    }
}

macro_rules! define_tagged_union {
    ($name:ident, $subtype_enum:ident, $downcastable_trait:ident, { $($member:ident),* }) => {
        #[repr(C, align(16))]
        pub struct $name {
            pub header: Header,
        }

        impl $name {
            pub fn downcast_ref<T>(&self) -> Option<&T>
            where
                T: $downcastable_trait,
            {
                if T::has_tag(self.header.type_tag) {
                    Some(unsafe { &*(self as *const $name as *const T) })
                } else {
                    None
                }
            }

            fn as_subtype(&self) -> $subtype_enum {
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

        pub trait $downcastable_trait : AnyDowncastable {}

        $(
            impl $downcastable_trait for $member {}
        )*

        impl AnyDowncastable for $name {
            fn has_tag(type_tag: TypeTag) -> bool {
                [$(TypeTag::$member),*].contains(&type_tag)
            }
        }

        pub enum $subtype_enum<'a> {
            $($member(&'a $member)),*
        }
    };
}

define_tagged_boxes! {
    Float,
    Int,
    Str,
    Pair,
    Nil
}

define_tagged_union!(Num, NumSubtype, NumDowncastable, {
    Int,
    Float
});

define_tagged_union!(List, ListSubtype, ListDowncastable, {
    Pair,
    Nil
});

impl List {
    pub fn list_length(&self) -> usize {
        match self.as_subtype() {
            ListSubtype::Pair(pair) => pair.list_length,
            ListSubtype::Nil(_) => 0,
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
    }

    #[test]
    fn downcast_ref() {
        let stack_float = Float::new(2.0);
        let stack_float_as_any: &Any = unsafe { &*(&stack_float as *const Float as *const Any) };

        assert_eq!(false, stack_float_as_any.downcast_ref::<Int>().is_some());
        assert_eq!(true, stack_float_as_any.downcast_ref::<Float>().is_some());
    }

    #[test]
    fn as_tagged() {
        let stack_float = Float::new(2.0);
        let stack_float_as_any: &Any = unsafe { &*(&stack_float as *const Float as *const Any) };

        if let AnySubtype::Float(_) = stack_float_as_any.as_subtype() {
        } else {
            panic!("Failed to get tagged representation")
        }
    }

    #[test]
    fn union_types() {
        let stack_float = Float::new(2.0);
        let stack_float_as_any: &Any = unsafe { &*(&stack_float as *const Float as *const Any) };

        if let Some(stack_num) = stack_float_as_any.downcast_ref::<Num>() {
            if let NumSubtype::Float(_) = stack_num.as_subtype() {
            } else {
                panic!("Couldn't get tagged Float from Num");
            }

            assert_eq!(false, stack_num.downcast_ref::<Int>().is_some());
            assert_eq!(true, stack_num.downcast_ref::<Float>().is_some());
        } else {
            panic!("Float was not a Num");
        }

        let stack_str = Str::new("Test!");
        let stack_str_as_any: &Any = unsafe { &*(&stack_str as *const Str as *const Any) };

        assert_eq!(false, stack_str_as_any.downcast_ref::<Num>().is_some());
    }
}
