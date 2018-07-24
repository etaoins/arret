use std::fmt;

use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use intern::Interner;

#[repr(C, align(16))]
pub struct Int {
    header: Header,
    value: i64,
}

impl ConstructableFrom<i64> for Int {
    fn size_for_value(_: &i64) -> BoxSize {
        BoxSize::Size16
    }

    fn construct(value: i64, alloc_type: AllocType, _: &mut Interner) -> Int {
        Int {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            value,
        }
    }
}

impl Int {
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Int) -> bool {
        self.value() == other.value()
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(formatter, "Int({:?})", self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Int>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::new();

        let boxed_one1 = Int::new(&mut heap, 1);
        let boxed_one2 = Int::new(&mut heap, 1);
        let boxed_two = Int::new(&mut heap, 2);

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::new();

        let boxed_one = Int::new(&mut heap, 1);
        assert_eq!("Int(1)", format!("{:?}", boxed_one));
    }
}
