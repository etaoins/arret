use boxed::{Any, ConstructableFrom, Gc, Header, HeapSize, List};

#[repr(C, align(16))]
pub struct Pair {
    pub header: Header,
    pub car: Gc<Any>,
    pub cdr: Gc<List>,
    pub list_length: usize,
}

type PairInput = (Gc<Any>, Gc<List>);

impl ConstructableFrom<PairInput> for Pair {
    fn heap_size_for_value(_: &PairInput) -> HeapSize {
        HeapSize::Size32
    }

    fn new_with_header(value: PairInput, header: Header) -> Pair {
        Pair {
            header,
            car: value.0,
            cdr: value.1,
            list_length: value.1.list_length() + 1,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Pair>());
    }
}
