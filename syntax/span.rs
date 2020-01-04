#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    file_id: Option<codespan::FileId>,
    codespan_span: codespan::Span,
}

impl Span {
    pub const fn new(file_id: Option<codespan::FileId>, codespan_span: codespan::Span) -> Self {
        Self {
            file_id,
            codespan_span,
        }
    }

    pub fn file_id(&self) -> Option<codespan::FileId> {
        self.file_id
    }

    pub fn start(&self) -> codespan::ByteIndex {
        self.codespan_span.start()
    }

    pub fn end(&self) -> codespan::ByteIndex {
        self.codespan_span.end()
    }

    pub fn codespan_span(&self) -> codespan::Span {
        self.codespan_span
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
        let byte_pos = (zero_size_off + 1) as u32;
        (byte_pos, byte_pos)
    } else {
        let start = v.find('^').expect("Positioning character not found") as u32;
        let end = v.rfind('^').map(|i| i + 1).unwrap() as u32;

        (start, end)
    };

    let codespan_span = codespan::Span::new(start, end);
    Span::new(None, codespan_span)
}
