use boxed::{ConstructableFrom, Header, HeapSize};

#[repr(C)]
pub struct Int {
    pub header: Header,
    pub value: i64,
}

impl ConstructableFrom<i64> for Int {
    fn heap_size_for_value(_: i64) -> HeapSize {
        HeapSize::Size16
    }

    fn new_with_header(value: i64, header: Header) -> Int {
        Int { header, value }
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
