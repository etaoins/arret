pub use codespan::ByteIndex;

pub type Span = codespan::ByteSpan;

pub const EMPTY_SPAN: Span = Span::new_unchecked(ByteIndex(0), ByteIndex(0));

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
