use std::{fmt, mem};

use crate::boxed::refs::Gc;
use crate::boxed::{AllocType, Any, BoxSize, ConstructableFrom, DirectTagged, Header};
use crate::intern::Interner;
use crate::task;

// TODO: This is a placeholder until we support record types
pub struct Record {}

pub type ThunkEntry = extern "C" fn(&mut task::Task, *const Record, Gc<Any>) -> Gc<Any>;

#[repr(C, align(16))]
pub struct FunThunk {
    header: Header,
    closure: *const Record,
    entry: ThunkEntry,
}

type FunThunkInput = (*const Record, ThunkEntry);

impl ConstructableFrom<FunThunkInput> for FunThunk {
    fn size_for_value(_: &FunThunkInput) -> BoxSize {
        if mem::size_of::<Self>() == 16 {
            BoxSize::Size16
        } else if mem::size_of::<Self>() == 32 {
            BoxSize::Size32
        } else {
            unreachable!("Unsupported fun size!")
        }
    }

    fn construct(value: FunThunkInput, alloc_type: AllocType, _: &mut Interner) -> FunThunk {
        FunThunk {
            header: Header {
                type_tag: Self::TYPE_TAG,
                alloc_type,
            },
            closure: value.0,
            entry: value.1,
        }
    }
}

impl FunThunk {
    pub fn apply(&mut self, task: &mut task::Task, arg_list: Gc<Any>) -> Gc<Any> {
        (self.entry)(task, self.closure, arg_list)
    }
}

impl PartialEq for FunThunk {
    fn eq(&self, other: &FunThunk) -> bool {
        self as *const _ == other as *const _
    }
}

impl fmt::Debug for FunThunk {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(formatter, "FunThunk({:p})", self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use crate::boxed::prelude::*;
    use std::mem;

    extern "C" fn identity_entry(
        _: &mut task::Task,
        _closure: *const Record,
        rest: Gc<Any>,
    ) -> Gc<Any> {
        rest
    }

    extern "C" fn return_42_entry(
        task: &mut task::Task,
        _closure: *const Record,
        _rest: Gc<Any>,
    ) -> Gc<Any> {
        use crate::boxed::Int;
        Int::new(task, 32).as_any_ref()
    }

    #[test]
    fn sizes() {
        assert!([16, 32].contains(&mem::size_of::<FunThunk>()));
    }

    #[test]
    fn equality() {
        use std::ptr;

        let mut heap = Heap::new();

        let boxed_identity1 = FunThunk::new(&mut heap, (ptr::null(), identity_entry));
        let boxed_identity2 = FunThunk::new(&mut heap, (ptr::null(), identity_entry));
        let boxed_return = FunThunk::new(&mut heap, (ptr::null(), return_42_entry));

        assert_ne!(boxed_identity1, boxed_return);
        // We use pointer identity for now
        assert_ne!(boxed_identity1, boxed_identity2);
    }
}