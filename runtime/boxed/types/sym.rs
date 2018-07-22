use boxed::{AllocType, BoxSize, ConstructableFrom, DirectTagged, Header};
use intern::{InternedSym, Interner};

#[repr(C, align(16))]
pub struct Sym {
    header: Header,
    // TODO: We have room to fit a u32 hash value here which should help with re-interning heap
    // indexed symbols in new heaps
    pub(crate) interned: InternedSym,
}

impl<'a> ConstructableFrom<&'a str> for Sym {
    fn size_for_value(_: &&str) -> BoxSize {
        BoxSize::Size16
    }

    fn construct(value: &'a str, alloc_type: AllocType, interner: &mut Interner) -> Sym {
        Sym {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            interned: interner.intern(value),
        }
    }
}

impl Sym {
    pub fn name<'a>(&'a self, interner: &'a Interner) -> &'a str {
        interner.unintern(&self.interned)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Sym>());
    }
}
