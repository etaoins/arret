use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ByteIndex(pub u32);

impl ByteIndex {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Copy)]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub fn new(start: ByteIndex, end: ByteIndex) -> Span {
        Span { start, end }
    }

    pub fn start(self) -> ByteIndex {
        self.start
    }

    pub fn end(self) -> ByteIndex {
        self.end
    }

    pub fn with_start(self, start: ByteIndex) -> Span {
        Span {
            start,
            end: self.end,
        }
    }

    pub fn to_non_empty(self) -> Option<Span> {
        if self.start != self.end {
            Some(self)
        } else {
            None
        }
    }

    pub fn contains(self, other: Span) -> bool {
        (other.start >= self.start) && (other.end <= self.end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start == self.end {
            write!(f, "Span[]")
        } else {
            write!(f, "Span[{}:{}]", self.start.0, self.end.0)
        }
    }
}

pub const EMPTY_SPAN: Span = Span {
    start: ByteIndex(0),
    end: ByteIndex(0),
};

// This isn't #[cfg(test)] because it's used in other crates
pub fn t2s(v: &str) -> Span {
    if let Some(zero_size_off) = v.find('>') {
        let byte_pos = (zero_size_off + 1) as u32;

        return Span::new(ByteIndex(byte_pos), ByteIndex(byte_pos));
    }

    let start = v.find('^').expect("Positioning character not found") as u32;
    let end = v.rfind('^').map(|i| i + 1).unwrap() as u32;

    Span::new(ByteIndex(start), ByteIndex(end))
}
