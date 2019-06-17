use std::fmt;
use std::hash::{Hash, Hasher};

use crate::boxed::*;
use crate::intern::{AsInterner, InternedSym};

/// Interned symbol
///
/// Symbols are immutable strings typically used as keywords or identifiers.
#[repr(C, align(16))]
pub struct Sym {
    header: Header,
    // TODO: We have room to fit a u32 hash value here which should help with re-interning heap
    // indexed symbols in new heaps
    pub(crate) interned: InternedSym,
}

impl Boxed for Sym {}
impl UniqueTagged for Sym {}

impl Sym {
    /// Constructs a new symbol with a specified name
    pub fn new(heap: &mut impl AsHeap, value: &str) -> Gc<Sym> {
        let heap = heap.as_heap_mut();
        let interned = heap.type_info_mut().interner_mut().intern(value);

        heap.place_box(Sym {
            header: Self::TYPE_TAG.to_heap_header(Self::size()),
            interned,
        })
    }

    /// Returns the box size for symbols
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }

    /// Returns the name of the symbol
    ///
    /// `interner` is required to unintern the name. It must be the same interner used to construct
    /// the symbol.
    pub fn name<'a>(&'a self, interner: &'a impl AsInterner) -> &'a str {
        interner.as_interner().unintern(&self.interned)
    }

    /// Returns the interned symbol value
    pub fn interned(&self) -> InternedSym {
        self.interned
    }
}

impl PartialEq for Sym {
    fn eq(&self, other: &Sym) -> bool {
        self.interned == other.interned
    }
}

impl Hash for Sym {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        self.interned.hash(state);
    }
}

impl fmt::Debug for Sym {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Sym({:?})", self.interned)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Sym>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let boxed_one1 = Sym::new(&mut heap, "one");
        let boxed_one2 = Sym::new(&mut heap, "one");
        let boxed_two = Sym::new(&mut heap, "two");

        assert_ne!(boxed_one1, boxed_two);
        assert_eq!(boxed_one1, boxed_one2);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Sym::new(&mut heap, "one");
        assert_eq!(r#"Sym('one)"#, format!("{:?}", boxed_one));
    }
}
