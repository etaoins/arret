use std::sync::Arc;
use std::{mem, ptr};

use boxed::{ConstructableFrom, Header, HeapSize};

#[repr(C, align(16))]
pub struct Str {
    pub header: Header,
    pub inline_byte_length: u8,
    pub padding: [u8; Str::MAX_INLINE_BYTES],
}

impl Str {
    const MAX_INLINE_BYTES: usize = 29;

    fn is_inline(&self) -> bool {
        self.inline_byte_length <= Str::MAX_INLINE_BYTES as u8
    }

    fn as_repr(&self) -> Repr {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Str as *const InlineStr) })
        } else {
            Repr::Shared(unsafe { &*(self as *const Str as *const SharedStr) })
        }
    }

    pub fn as_str(&self) -> &str {
        match self.as_repr() {
            Repr::Inline(inline) => inline.as_str(),
            Repr::Shared(shared) => shared.shared_str.as_ref(),
        }
    }
}

impl<'a> ConstructableFrom<&'a str> for Str {
    fn heap_size_for_value(value: &&str) -> HeapSize {
        match value.len() {
            0..=13 => HeapSize::Size16,
            14..=Str::MAX_INLINE_BYTES => HeapSize::Size32,
            _ => {
                // Too big for the heap; this needs to be shared
                HeapSize::Size32
            }
        }
    }

    fn new_with_header(value: &str, header: Header) -> Str {
        unsafe {
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
        }
    }
}

impl Drop for Str {
    fn drop(&mut self) {
        // ptr::read will properly drop our specific representations
        match self.as_repr() {
            Repr::Inline(inline) => unsafe {
                ptr::read(inline);
            },
            Repr::Shared(shared) => unsafe {
                ptr::read(shared);
            },
        }
    }
}

#[repr(C, align(16))]
struct InlineStr {
    pub header: Header,
    pub inline_byte_length: u8,
    pub inline_bytes: [u8; 29],
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
    pub header: Header,
    // Once we've determined we're not inline this has no useful value
    pub inline_byte_length: u8,
    pub shared_str: Arc<str>,
    pub padding: [u64; 1],
}

enum Repr<'a> {
    Inline(&'a InlineStr),
    Shared(&'a SharedStr),
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<SharedStr>());
        assert_eq!(32, mem::size_of::<InlineStr>());
        assert_eq!(32, mem::size_of::<Str>());
    }

    #[test]
    fn round_trip() {
        for &test_str in &[
            "",
            "1",
            "smallinline",
            "largerinlinethattakes32bytes",
            "This definitely will not fit in any inline string",
        ] {
            let boxed_string = Str::new(test_str);
            assert_eq!(test_str, boxed_string.as_str());
        }
    }
}
