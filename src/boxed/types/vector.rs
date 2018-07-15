use boxed::{Any, ConstructableFrom, Gc, Header, HeapSize, TypeTag, TypeTagged};

#[repr(C, align(16))]
pub struct Vector<T> {
    pub header: Header,
    pub values: Vec<Gc<T>>,
}

impl<T> TypeTagged for Vector<T> {
    const TYPE_TAG: TypeTag = TypeTag::TopVector;
}

impl<'a, T> ConstructableFrom<&'a [Gc<T>]> for Vector<T> {
    fn heap_size_for_value(_: &&[Gc<T>]) -> HeapSize {
        HeapSize::Size32
    }

    fn new_with_header(values: &[Gc<T>], header: Header) -> Vector<T> {
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
