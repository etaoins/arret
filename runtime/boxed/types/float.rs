use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use intern::Interner;

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
    pub fn value(&self) -> f64 {
        self.value
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
