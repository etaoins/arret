use std::collections::HashMap;
use std::{ptr, str};

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
    padding: [u8; 3],
    name_idx: u32,
}

#[repr(align(8))]
#[derive(Copy, Clone)]
struct InternedInline {
    name_bytes: [u8; INLINE_SIZE],
}

#[repr(align(8))]
#[derive(Copy, Clone)]
pub union InternedSym {
    indexed: InternedIndexed,
    inline: InternedInline,
}

impl PartialEq for InternedSym {
    fn eq(&self, other: &InternedSym) -> bool {
        // This simply compares our 8 byte representation directly
        // LLVM is clever enough to compare using a single instruction on x86-64/ARM64
        unsafe { self.inline.name_bytes == other.inline.name_bytes }
    }
}

impl Eq for InternedSym {}

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
                    padding: [0; 3],
                    name_idx: index as u32,
                },
            }
        }
    }

    // We need to take a reference so we can return a reference
    // See rust-lang-nursery/rust-clippy#2946C
    #[cfg_attr(feature = "cargo-clippy", allow(trivially_copy_pass_by_ref))]
    pub fn unintern<'a>(&'a self, interned: &'a InternedSym) -> &'a str {
        unsafe {
            if interned.indexed.flag_byte == INDEXED_FLAG {
                // This is indexed
                &self.names[interned.indexed.name_idx as usize]
            } else {
                // Find the first fill byte. If none is found assume our full inline size.
                let length = interned
                    .inline
                    .name_bytes
                    .iter()
                    .position(|byte| *byte == INLINE_FILL_BYTE)
                    .unwrap_or(INLINE_SIZE);

                str::from_utf8_unchecked(&interned.inline.name_bytes[0..length])
            }
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
        assert!(intern_inline1 == intern_inline2);

        let intern_index1 = interner.intern(index_name);
        let intern_index2 = interner.intern(index_name);
        assert!(intern_index1 == intern_index2);

        // These should not be equal
        assert!(intern_inline1 != intern_index1);
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
