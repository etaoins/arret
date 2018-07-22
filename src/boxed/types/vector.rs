use abitype::{BoxedABIType, EncodeBoxedABIType};
use boxed::refs::Gc;
use boxed::{AllocType, Any, BoxSize, Boxed, ConstructableFrom, Header, TypeTag};
use intern::Interner;

#[repr(C, align(16))]
pub struct Vector<T>
where
    T: Boxed,
{
    pub header: Header,
    pub values: Vec<Gc<T>>,
}

impl<T> Boxed for Vector<T> where T: Boxed {}

impl<'a, T> ConstructableFrom<&'a [Gc<T>]> for Vector<T>
where
    T: Boxed,
{
    fn size_for_value(_: &&[Gc<T>]) -> BoxSize {
        BoxSize::Size32
    }

    fn construct(values: &[Gc<T>], alloc_type: AllocType, _: &mut Interner) -> Vector<T> {
        Vector {
            header: Header {
                type_tag: TypeTag::TopVector,
                alloc_type,
            },
            values: values.into(),
        }
    }
}

impl<T> EncodeBoxedABIType for Vector<T>
where
    T: EncodeBoxedABIType + Boxed,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Vector(&T::BOXED_ABI_TYPE);
}

#[repr(C, align(16))]
pub struct TopVector {
    pub header: Header,
}

impl TopVector {
    fn as_vector(&self) -> Gc<Vector<Any>> {
        unsafe { Gc::new(&*(self as *const TopVector as *const Vector<Any>)) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Vector<Any>>());
    }
}
