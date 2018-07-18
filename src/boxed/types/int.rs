use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};

#[repr(C, align(16))]
pub struct Int {
    pub header: Header,
    pub value: i64,
}

impl ConstructableFrom<i64> for Int {
    fn size_for_value(_: &i64) -> BoxSize {
        BoxSize::Size16
    }

    fn new_with_alloc_type(value: i64, alloc_type: AllocType) -> Int {
        Int {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            value,
        }
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
