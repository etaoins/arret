use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::{fmt, ptr, str};

///! Interned symbols
///!
///! This uses a fixed 8 byte representation for interned symbol. They are associated with a
///! particular `Interner` instance which can return the original `str` name of the symbol.
///! Interned symbols from the same `Interner` can be compared directly without a reference to
///! the `Interner` instance.
///!
///! Symbol names of 8 bytes or less are encoded directly in the `InternedSym` instance without
///! storing the name in the `Interner`. They are padded with a constant invalid UTF-8 sequence so
///! the length of the inline name can be recovered.
///!
///! The encoding for names larger than 8 bytes uses an index in to a `Vec` stored in the
///! `Interner`. The indexed representation is invalid UTF-8 so it cannot collide with a valid
///! symbol name.

// UTF-8 sequences cannot start with 10xxxxxxx. This is pattern for the last continuation byte,
// but any 1 byte sequences are encoded directly. We can use these values freely without colliding
// with inline names.
const INLINE_FILL_BYTE: u8 = 0x80;
const INDEXED_FLAG: u8 = 0x81;

const INLINE_SIZE: usize = 8;

#[repr(align(8))]
#[derive(Copy, Clone)]
struct InternedIndexed {
    flag_byte: u8,
    _padding: [u8; 3],
    name_idx: u32,
}

#[repr(align(8))]
#[derive(Copy, Clone)]
struct InternedInline {
    name_bytes: [u8; INLINE_SIZE],
}

impl InternedInline {
    fn as_str(&self) -> &str {
        // Find the first fill byte. If none is found assume our full inline size.
        let length = self
            .name_bytes
            .iter()
            .position(|byte| *byte == INLINE_FILL_BYTE)
            .unwrap_or(INLINE_SIZE);

        unsafe { str::from_utf8_unchecked(&self.name_bytes[0..length]) }
    }
}

#[repr(align(8))]
#[derive(Copy, Clone)]
pub union InternedSym {
    indexed: InternedIndexed,
    inline: InternedInline,
}

enum InternedRepr<'a> {
    Inline(&'a InternedInline),
    Indexed(&'a InternedIndexed),
}

impl InternedSym {
    fn repr(&self) -> InternedRepr<'_> {
        unsafe {
            if self.indexed.flag_byte == INDEXED_FLAG {
                InternedRepr::Indexed(&self.indexed)
            } else {
                InternedRepr::Inline(&self.inline)
            }
        }
    }
}

impl PartialEq for InternedSym {
    fn eq(&self, other: &InternedSym) -> bool {
        // This simply compares our 8 byte representation directly
        // LLVM is clever enough to compare using a single instruction on x86-64/ARM64
        unsafe { self.inline.name_bytes == other.inline.name_bytes }
    }
}

impl Eq for InternedSym {}

impl Hash for InternedSym {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            state.write(&self.inline.name_bytes);
        }
    }
}

impl fmt::Debug for InternedSym {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.repr() {
            InternedRepr::Indexed(indexed) => {
                // We don't have access to the `Interner` so we can't print our interned value
                write!(formatter, "`{:x}", indexed.name_idx)
            }
            InternedRepr::Inline(inline) => write!(formatter, "'{}", inline.as_str()),
        }
    }
}

// TODO: This keeps two copies of the name. We can't simply keep a pointer to inside the `Vec`
// or `HashMap` as they might reallocate. We can fix this later.
pub struct Interner {
    names: Vec<Box<str>>,
    name_to_idx: HashMap<Box<str>, usize>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            names: vec![],
            name_to_idx: HashMap::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> InternedSym {
        if name.len() <= INLINE_SIZE {
            let mut interned_inline = InternedInline {
                name_bytes: [INLINE_FILL_BYTE; INLINE_SIZE],
            };

            unsafe {
                ptr::copy_nonoverlapping(
                    name.as_ptr(),
                    &mut interned_inline.name_bytes[0] as *mut u8,
                    name.len(),
                );
            }

            InternedSym {
                inline: interned_inline,
            }
        } else {
            let index = self.name_to_idx.get(name).cloned().unwrap_or_else(|| {
                let index = self.names.len();
                self.names.push(name.into());
                self.name_to_idx.insert(name.into(), index);

                index
            });

            InternedSym {
                indexed: InternedIndexed {
                    flag_byte: INDEXED_FLAG,
                    _padding: [0; 3],
                    name_idx: index as u32,
                },
            }
        }
    }

    pub fn unintern<'a>(&'a self, interned: &'a InternedSym) -> &'a str {
        match interned.repr() {
            InternedRepr::Indexed(indexed) => &self.names[indexed.name_idx as usize],
            InternedRepr::Inline(inline) => inline.as_str(),
        }
    }
}

impl Default for Interner {
    fn default() -> Interner {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(8, mem::size_of::<InternedIndexed>());
        assert_eq!(8, mem::size_of::<InternedInline>());
        assert_eq!(8, mem::size_of::<InternedSym>());
    }

    #[test]
    fn equality() {
        let inline_name = "inline";
        let index_name = "This must be longer than eight bytes";

        let mut interner = Interner::new();

        let intern_inline1 = interner.intern(inline_name);
        let intern_inline2 = interner.intern(inline_name);
        assert_eq!(intern_inline1, intern_inline2);

        let intern_index1 = interner.intern(index_name);
        let intern_index2 = interner.intern(index_name);
        assert_eq!(intern_index1, intern_index2);

        // These should not be equal
        assert_ne!(intern_inline1, intern_index1);
    }

    #[test]
    fn fmt_debug() {
        let mut interner = Interner::new();

        let intern_inline = interner.intern("inline");
        assert_eq!("'inline", format!("{:?}", intern_inline));

        let intern_indexed = interner.intern("This is very long and can't be stored inline");
        assert_eq!("`0", format!("{:?}", intern_indexed));
    }

    #[test]
    fn roundtrip() {
        let mut interner = Interner::new();

        let test_names = [
            "",
            "short1",
            "short2",
            "exactly8",
            "Hello, world!",
            "This is another long test string",
        ];

        let mut previous_interneds = vec![];
        for &name in &test_names {
            let interned = interner.intern(name);
            assert_eq!(name, interner.unintern(&interned));

            // Make sure we don't equal any of our previous interneds
            assert!(!previous_interneds.contains(&interned));
            previous_interneds.push(interned);
        }
    }
}
