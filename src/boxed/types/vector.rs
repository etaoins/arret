use boxed::refs::BoxRef;
use boxed::{Any, ConstructableFrom, Header, HeapSize, TypeTag, TypeTagged};

#[repr(C, align(16))]
pub struct Vector<T>
where
    T: BoxRef,
{
    pub header: Header,
    pub values: Vec<T>,
}

impl<T> TypeTagged for Vector<T>
where
    T: BoxRef,
{
    const TYPE_TAG: TypeTag = TypeTag::TopVector;
}

impl<'a, T> ConstructableFrom<&'a [T]> for Vector<T>
where
    T: BoxRef,
{
    fn heap_size_for_value(_: &&[T]) -> HeapSize {
        HeapSize::Size32
    }

    fn new_with_header(values: &[T], header: Header) -> Vector<T> {
        Vector {
            header,
            values: values.into(),
        }
    }
}

#[repr(C, align(16))]
pub struct TopVector {
    pub header: Header,
}

impl TopVector {
    fn as_vector<T>(&self) -> &Vector<T>
    where
        T: BoxRef<Target = Any>,
    {
        unsafe { &*(self as *const TopVector as *const Vector<T>) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use boxed::refs::Gc;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Vector<Gc<Any>>>());
    }
}
