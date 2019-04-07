use std::fmt;
use std::hash::{Hash, Hasher};

use crate::boxed::refs::Gc;
use crate::boxed::*;
use crate::intern::Interner;

#[repr(C, align(16))]
pub struct Float {
    header: Header,
    value: f64,
}

impl Boxed for Float {
    fn header(&self) -> Header {
        self.header
    }
}

impl UniqueTagged for Float {}

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

    pub fn as_num(&self) -> Gc<Num> {
        unsafe { Gc::new(&*(self as *const _ as *const Num)) }
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Float) -> bool {
        self.value() == other.value()
    }
}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);

        let value = self.value();
        if value == 0.0 {
            // 0.0 == -0.0 so they need to hash to the same value
            state.write_u64((0.0f64).to_bits())
        } else {
            // NaNs will mostly map to the same value which is allowed but also a collision
            state.write_u64(value.to_bits());
        }
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

    fn calc_hash(value: f64) -> u64 {
        use std::collections::hash_map::DefaultHasher;

        let mut heap = Heap::empty();
        let boxed_float = Float::new(&mut heap, value);

        let mut hasher = DefaultHasher::new();
        boxed_float.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Float>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let boxed_one1 = Float::new(&mut heap, 1.0);
        let boxed_one2 = Float::new(&mut heap, 1.0);
        let boxed_two = Float::new(&mut heap, 2.0);

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn hash() {
        let minus_zero_hash = calc_hash(-0.0);
        let plus_zero_hash = calc_hash(0.0);
        let plus_one_hash = calc_hash(1.0);

        assert_ne!(plus_one_hash, minus_zero_hash);
        assert_eq!(plus_zero_hash, minus_zero_hash);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Float::new(&mut heap, 1.0);
        assert_eq!("Float(1.0)", format!("{:?}", boxed_one));
    }
}
