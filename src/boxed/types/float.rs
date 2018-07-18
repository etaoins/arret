use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};

#[repr(C, align(16))]
pub struct Float {
    pub header: Header,
    pub value: f64,
}

impl ConstructableFrom<f64> for Float {
    fn size_for_value(_: &f64) -> BoxSize {
        BoxSize::Size16
    }

    fn new_with_alloc_type(value: f64, alloc_type: AllocType) -> Float {
        Float {
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
        assert_eq!(16, mem::size_of::<Float>());
    }
}
