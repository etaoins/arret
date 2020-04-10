use std::num::NonZeroU32;
use std::ops::Range;

pub type FileId = NonZeroU32;
pub type ByteIndex = u32;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    file_id: Option<FileId>,
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub const fn new(file_id: Option<FileId>, start: ByteIndex, end: ByteIndex) -> Self {
        Self {
            file_id,
            start,
            end,
        }
    }

    pub const fn from_str(file_id: Option<FileId>, s: &str) -> Self {
        Self {
            file_id,
            start: 0,
            end: s.len() as ByteIndex,
        }
    }

    pub fn file_id(&self) -> Option<FileId> {
        self.file_id
    }

    pub fn start(&self) -> ByteIndex {
        self.start
    }

    pub fn end(&self) -> ByteIndex {
        self.end
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    pub fn contains(&self, other: Span) -> bool {
        self.file_id == other.file_id && self.start() <= other.start() && self.end() >= other.end()
    }
}

// This isn't #[cfg(test)] because it's used in other crates
pub fn t2s(v: &str) -> Span {
    let (start, end) = if v.is_empty() {
        // Used for empty files
        (0, 0)
    } else if let Some(zero_size_off) = v.find('>') {
        let byte_pos = (zero_size_off + 1) as ByteIndex;
        (byte_pos, byte_pos)
    } else {
        let start = v.find('^').expect("Positioning character not found") as ByteIndex;
        let end = v.rfind('^').map(|i| i + 1).unwrap() as ByteIndex;

        (start, end)
    };

    Span::new(None, start, end)
}
