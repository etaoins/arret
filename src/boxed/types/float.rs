use boxed::{BoxSize, ConstructableFrom, Header};

#[repr(C, align(16))]
pub struct Float {
    pub header: Header,
    pub value: f64,
}

impl ConstructableFrom<f64> for Float {
    fn size_for_value(_: &f64) -> BoxSize {
        BoxSize::Size16
    }

    fn new_with_header(value: f64, header: Header) -> Float {
        Float { header, value }
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
