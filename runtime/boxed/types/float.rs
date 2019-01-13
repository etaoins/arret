use std::fmt;

use crate::boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use crate::intern::Interner;

#[repr(C, align(16))]
pub struct Float {
    header: Header,
    value: f64,
}

impl ConstructableFrom<f64> for Float {
    fn size_for_value(_: &f64) -> BoxSize {
        BoxSize::Size16
    }

    fn construct(value: f64, alloc_type: AllocType, _: &mut Interner) -> Float {
        Float {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            value,
        }
    }
}

impl Float {
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }
    
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Float) -> bool {
        self.value() == other.value()
    }
}

impl fmt::Debug for Float {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Float({:?})", self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Float>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::new();

        let boxed_one1 = Float::new(&mut heap, 1.0);
        let boxed_one2 = Float::new(&mut heap, 1.0);
        let boxed_two = Float::new(&mut heap, 2.0);

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::new();

        let boxed_one = Float::new(&mut heap, 1.0);
        assert_eq!("Float(1.0)", format!("{:?}", boxed_one));
    }
}
