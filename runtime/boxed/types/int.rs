use std::fmt;
use std::hash::{Hash, Hasher};

use crate::boxed::refs::Gc;
use crate::boxed::*;

/// Boxed 64bit signed integer
#[repr(C, align(16))]
pub struct Int {
    header: Header,
    value: i64,
}

impl Boxed for Int {}
impl UniqueTagged for Int {}

impl Int {
    /// Constructs a new integer
    pub fn new(heap: &mut impl AsHeap, value: i64) -> Gc<Int> {
        heap.as_heap_mut().place_box(Int {
            header: Self::TYPE_TAG.to_heap_header(Self::size()),
            value,
        })
    }

    /// Returns the box size for integers
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }

    /// Returns the unboxed value of this integer
    pub fn value(&self) -> i64 {
        self.value
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Int) -> bool {
        self.value() == other.value()
    }
}

impl Hash for Int {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        self.value().hash(state)
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Int({:?})", self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Int>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let boxed_one1 = Int::new(&mut heap, 1);
        let boxed_one2 = Int::new(&mut heap, 1);
        let boxed_two = Int::new(&mut heap, 2);

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Int::new(&mut heap, 1);
        assert_eq!("Int(1)", format!("{:?}", boxed_one));
    }
}
