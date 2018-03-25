#[derive(PartialEq, Eq, Debug, Hash, Clone, PartialOrd, Ord, Copy)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn with_lo(&self, lo: u32) -> Span {
        Span {
            lo: lo,
            hi: self.hi,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.lo == self.hi
    }
}

pub const EMPTY_SPAN: Span = Span { lo: 0, hi: 0 };

#[cfg(test)]
pub fn t2s(v: &str) -> Span {
    if let Some(zero_size_off) = v.find('>') {
        let byte_pos = (zero_size_off + 1) as u32;

        return Span {
            lo: byte_pos,
            hi: byte_pos,
        };
    }

    let lo = v.find('^').expect("Positioning character not found") as u32;
    let hi = v.rfind('^').map(|i| i + 1).unwrap() as u32;

    Span { lo, hi }
}
