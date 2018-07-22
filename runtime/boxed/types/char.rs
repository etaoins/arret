use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use intern::Interner;

#[repr(C, align(16))]
pub struct Char {
    pub header: Header,
    pub value: char,
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

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Char>());
    }
}
