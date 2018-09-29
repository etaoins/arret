use std::fmt;

use crate::boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use crate::intern::Interner;

#[repr(C, align(16))]
pub struct Char {
    header: Header,
    value: char,
}

impl ConstructableFrom<char> for Char {
    fn size_for_value(_: &char) -> BoxSize {
        BoxSize::Size16
    }

    fn construct(value: char, alloc_type: AllocType, _: &mut Interner) -> Char {
        Char {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            value,
        }
    }
}

impl Char {
    pub fn value(&self) -> char {
        self.value
    }
}

impl PartialEq for Char {
    fn eq(&self, other: &Char) -> bool {
        self.value == other.value
    }
}

impl fmt::Debug for Char {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Char({:?})", self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Char>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::new();

        let boxed_a1 = Char::new(&mut heap, 'a');
        let boxed_a2 = Char::new(&mut heap, 'a');
        let boxed_b = Char::new(&mut heap, 'b');

        assert_ne!(boxed_a1, boxed_b);
        assert_eq!(boxed_a1, boxed_a2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::new();

        let boxed_a = Char::new(&mut heap, 'a');
        assert_eq!("Char('a')", format!("{:?}", boxed_a));
    }
}
