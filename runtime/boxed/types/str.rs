use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::{fmt, mem, ptr};

use crate::boxed::*;

/// String value encoded as UTF-8
#[repr(C, align(16))]
pub struct Str {
    header: Header,
    inline_byte_length: u8,
    padding: [u8; Str::MAX_INLINE_BYTES],
}

impl Boxed for Str {}
impl UniqueTagged for Str {}

impl Str {
    /// Maximum number of bytes that can be stored directly in the box
    pub const MAX_INLINE_BYTES: usize = 29;

    /// Constructs a new string
    pub fn new(heap: &mut impl AsHeap, value: &str) -> Gc<Str> {
        let box_size = match value.len() {
            0..=13 => BoxSize::Size16,
            14..=Str::MAX_INLINE_BYTES => BoxSize::Size32,
            _ => {
                // Too big to fit inline; this needs to be shared
                BoxSize::Size32
            }
        };

        let header = Self::TYPE_TAG.to_heap_header(box_size);

        let boxed = unsafe {
            if value.len() > Str::MAX_INLINE_BYTES {
                let shared_str = SharedStr {
                    header,
                    inline_byte_length: (Str::MAX_INLINE_BYTES as u8) + 1,
                    shared_str: value.into(),
                    padding: mem::uninitialized(),
                };

                mem::transmute(shared_str)
            } else {
                let mut inline_str = InlineStr {
                    header,
                    inline_byte_length: value.len() as u8,
                    inline_bytes: mem::uninitialized(),
                };
                ptr::copy(
                    value.as_ptr(),
                    &mut inline_str.inline_bytes[0] as *mut u8,
                    value.len(),
                );

                mem::transmute(inline_str)
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    fn is_inline(&self) -> bool {
        self.inline_byte_length <= Str::MAX_INLINE_BYTES as u8
    }

    fn as_repr(&self) -> Repr<'_> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Str as *const InlineStr) })
        } else {
            Repr::Shared(unsafe { &*(self as *const Str as *const SharedStr) })
        }
    }

    /// Returns the string's content as a slice
    pub fn as_str(&self) -> &str {
        match self.as_repr() {
            Repr::Inline(inline) => inline.as_str(),
            Repr::Shared(shared) => shared.shared_str.as_ref(),
        }
    }
}

impl PartialEq for Str {
    fn eq(&self, other: &Str) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Hash for Str {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        self.as_str().hash(state);
    }
}

impl fmt::Debug for Str {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Str({:?})", self.as_str())
    }
}

impl Drop for Str {
    fn drop(&mut self) {
        match self.as_repr() {
            Repr::Inline(_) => {
                // Do nothing here; we might've been allocated as a 16 byte box so we can't read
                // the whole thing.
            }
            Repr::Shared(shared) => unsafe {
                // ptr::read will properly drop our Rust fields
                ptr::read(shared);
            },
        }
    }
}

#[repr(C, align(16))]
struct InlineStr {
    header: Header,
    inline_byte_length: u8,
    inline_bytes: [u8; Str::MAX_INLINE_BYTES],
}

impl InlineStr {
    fn as_utf8(&self) -> &[u8] {
        use std::slice;
        unsafe {
            slice::from_raw_parts(
                &self.inline_bytes[0] as *const u8,
                self.inline_byte_length as usize,
            )
        }
    }

    fn as_str(&self) -> &str {
        use std::str;
        unsafe { str::from_utf8_unchecked(self.as_utf8()) }
    }
}

#[repr(C, align(16))]
struct SharedStr {
    header: Header,
    // Once we've determined we're not inline this has no useful value
    inline_byte_length: u8,
    shared_str: Arc<str>,
    padding: [u64; 1],
}

enum Repr<'a> {
    Inline(&'a InlineStr),
    Shared(&'a SharedStr),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<SharedStr>());
        assert_eq!(32, mem::size_of::<InlineStr>());
        assert_eq!(32, mem::size_of::<Str>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let boxed_one1 = Str::new(&mut heap, "one");
        let boxed_one2 = Str::new(&mut heap, "one");
        let boxed_two = Str::new(&mut heap, "two");

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Str::new(&mut heap, "one");
        assert_eq!(r#"Str("one")"#, format!("{:?}", boxed_one));
    }

    #[test]
    fn round_trip() {
        let mut heap = Heap::empty();

        for &test_str in &[
            "",
            "1",
            "smallinline",
            "largerinlinethattakes32bytes",
            "This definitely will not fit in any inline string",
        ] {
            let boxed_string = Str::new(&mut heap, test_str);
            assert_eq!(test_str, boxed_string.as_str());
        }
    }
}
