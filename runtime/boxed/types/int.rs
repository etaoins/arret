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

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Int>());
    }
}
