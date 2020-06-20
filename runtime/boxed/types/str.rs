use std::hash::{Hash, Hasher};
use std::mem::MaybeUninit;
use std::{fmt, mem, ptr};

use crate::boxed::types::shared_str::SharedStr;
use crate::boxed::*;

/// Describes the storage of a string's data
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StrStorage {
    /// String data is stored inline in a box of the given size
    Inline(BoxSize),
    /// String data is stored out-of-line in a 16 byte box
    External,
}

impl StrStorage {
    /// Returns the box size for a string storage
    pub fn box_size(self) -> BoxSize {
        match self {
            StrStorage::Inline(box_size) => box_size,
            StrStorage::External => BoxSize::Size16,
        }
    }
}

/// String value encoded as UTF-8
#[repr(C, align(16))]
pub struct Str {
    header: Header,
    inline_byte_len: u8,
    padding: [u8; Str::MAX_INLINE_BYTES],
}

impl Boxed for Str {}
impl UniqueTagged for Str {}

impl Str {
    /// Maximum number of bytes that can be stored directly in the box
    pub const MAX_INLINE_BYTES: usize = 29;

    /// Inline byte length used for external strings
    pub const EXTERNAL_INLINE_BYTE_LEN: u8 = (Self::MAX_INLINE_BYTES as u8) + 1;

    /// Constructs a new string
    pub fn new(heap: &mut impl AsHeap, value: &str) -> Gc<Str> {
        let storage = Self::storage_for_byte_len(value.len());
        let header = Self::TYPE_TAG.to_heap_header(storage.box_size());

        let boxed = unsafe {
            match storage {
                StrStorage::External => mem::transmute(ExternalStr::new(header, value)),
                StrStorage::Inline(_) => mem::transmute(InlineStr::new(header, value)),
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    /// Returns the storage for given string byte length
    pub fn storage_for_byte_len(len: usize) -> StrStorage {
        match len {
            0..=13 => StrStorage::Inline(BoxSize::Size16),
            14..=Str::MAX_INLINE_BYTES => StrStorage::Inline(BoxSize::Size32),
            _ => {
                // Too big to fit inline; this needs to be external
                StrStorage::External
            }
        }
    }

    fn is_inline(&self) -> bool {
        self.inline_byte_len != Self::EXTERNAL_INLINE_BYTE_LEN as u8
    }

    fn as_repr(&self) -> Repr<'_> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Str as *const InlineStr) })
        } else {
            Repr::External(unsafe { &*(self as *const Str as *const ExternalStr) })
        }
    }

    fn as_repr_mut(&mut self) -> ReprMut<'_> {
        if self.is_inline() {
            ReprMut::Inline(unsafe { &mut *(self as *mut Str as *mut InlineStr) })
        } else {
            ReprMut::External(unsafe { &mut *(self as *mut Str as *mut ExternalStr) })
        }
    }

    /// Returns the string's content as a slice
    pub fn as_str(&self) -> &str {
        match self.as_repr() {
            Repr::Inline(inline) => inline.as_str(),
            Repr::External(external) => external.shared_str.as_str(),
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
        match self.as_repr_mut() {
            ReprMut::Inline(_) => {
                // Do nothing here; we might've been allocated as a 16 byte box so we can't read
                // the whole thing.
            }
            ReprMut::External(external) => unsafe {
                ptr::drop_in_place(external);
            },
        }
    }
}

#[repr(C, align(16))]
struct InlineStr {
    header: Header,
    inline_byte_len: u8,
    inline_bytes: MaybeUninit<[u8; Str::MAX_INLINE_BYTES]>,
}

impl InlineStr {
    fn new(header: Header, value: &str) -> InlineStr {
        unsafe {
            let mut inline_bytes = mem::MaybeUninit::<[u8; Str::MAX_INLINE_BYTES]>::uninit();

            ptr::copy(
                value.as_ptr(),
                inline_bytes.as_mut_ptr() as *mut _,
                value.len(),
            );

            InlineStr {
                header,
                inline_byte_len: value.len() as u8,
                inline_bytes,
            }
        }
    }

    fn as_utf8(&self) -> &[u8] {
        use std::slice;
        unsafe {
            slice::from_raw_parts(
                self.inline_bytes.as_ptr() as *const u8,
                self.inline_byte_len as usize,
            )
        }
    }

    fn as_str(&self) -> &str {
        use std::str;
        unsafe { str::from_utf8_unchecked(self.as_utf8()) }
    }
}

#[repr(C, align(16))]
struct ExternalStr {
    header: Header,
    // Once we've determined we're not inline this has no useful value
    inline_byte_len: u8,
    shared_str: SharedStr,
    padding: [u64; 2],
}

impl ExternalStr {
    fn new(header: Header, value: &str) -> ExternalStr {
        ExternalStr {
            header,
            inline_byte_len: Str::EXTERNAL_INLINE_BYTE_LEN,
            shared_str: value.into(),
            padding: [0, 0],
        }
    }
}

enum Repr<'a> {
    Inline(&'a InlineStr),
    External(&'a ExternalStr),
}

enum ReprMut<'a> {
    Inline(&'a mut InlineStr),
    External(&'a mut ExternalStr),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<ExternalStr>());
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
