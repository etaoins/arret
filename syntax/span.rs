use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Copy)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn with_lo(self, lo: u32) -> Span {
        Span { lo, hi: self.hi }
    }

    pub fn to_non_empty(self) -> Option<Span> {
        if self.lo != self.hi {
            Some(self)
        } else {
            None
        }
    }

    pub fn contains(self, other: Span) -> bool {
        (other.lo >= self.lo) && (other.hi <= self.hi)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.lo == self.hi {
            write!(f, "Span[]")
        } else {
            write!(f, "Span[{}:{}]", self.lo, self.hi)
        }
    }
}

pub const EMPTY_SPAN: Span = Span { lo: 0, hi: 0 };

// This isn't #[cfg(test)] because it's used in other crates
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
