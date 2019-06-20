use std::fmt;
use std::hash::{Hash, Hasher};

use crate::boxed::refs::Gc;
use crate::boxed::*;
use crate::task;

/// Opaque type for a function's closure
///
/// This has a meaning specific to the implementation of the function. This may be a dummy value
/// (typically [`Nil`]) for functions without a closure, a single boxed value or a collection of
/// multiple boxed values. The only external contract is that it must be a boxed value to allow for
/// garbage collection.
pub type Closure = Gc<Any>;

/// Entry point for executing a function
pub type ThunkEntry = extern "C" fn(&mut task::Task, Closure, Gc<Any>) -> Gc<Any>;

/// Boxed function value with an optional closure
///
/// This is typically used in places where functions are used as values or stored in collections.
/// For example, placing a function in a list will create a `FunThunk`. When taking an function as a
/// parameter to an RFI function it's typically better to use a typed
/// [`callback::Callback`](crate::callback::Callback).
#[repr(C, align(16))]
pub struct FunThunk {
    header: Header,
    pub(crate) closure: Closure,
    entry: ThunkEntry,
}

impl Boxed for FunThunk {}
impl UniqueTagged for FunThunk {}

impl FunThunk {
    /// Constructs a new function value with the given closure and entry point
    pub fn new(heap: &mut impl AsHeap, closure: Closure, entry: ThunkEntry) -> Gc<FunThunk> {
        heap.as_heap_mut().place_box(FunThunk {
            header: Self::TYPE_TAG.to_heap_header(Self::size()),
            closure,
            entry,
        })
    }

    /// Returns the box size for functions
    pub fn size() -> BoxSize {
        BoxSize::Size32
    }

    /// Applies this function on the passed task with the given arguments
    pub fn apply(&self, task: &mut task::Task, arg_list: Gc<Any>) -> Gc<Any> {
        (self.entry)(task, self.closure, arg_list)
    }
}

impl PartialEq for FunThunk {
    fn eq(&self, _: &FunThunk) -> bool {
        // There is no reliable way to compare functions so they're always inequal
        false
    }
}

impl Eq for FunThunk {}

impl Hash for FunThunk {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        state.write_usize(self as *const _ as usize);
    }
}

impl fmt::Debug for FunThunk {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "FunThunk({:p})", self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed;
    use crate::boxed::heap::Heap;
    use std::mem;

    extern "C" fn identity_entry(_: &mut task::Task, _closure: Closure, rest: Gc<Any>) -> Gc<Any> {
        rest
    }

    extern "C" fn return_42_entry(
        task: &mut task::Task,
        _closure: Closure,
        _rest: Gc<Any>,
    ) -> Gc<Any> {
        Int::new(task, 32).as_any_ref()
    }

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<FunThunk>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let nil_closure = boxed::NIL_INSTANCE.as_any_ref();
        let boxed_identity1 = FunThunk::new(&mut heap, nil_closure, identity_entry);
        let boxed_identity2 = FunThunk::new(&mut heap, nil_closure, identity_entry);
        let boxed_return = FunThunk::new(&mut heap, nil_closure, return_42_entry);

        assert_ne!(boxed_identity1, boxed_return);
        // We use pointer identity for now
        assert_ne!(boxed_identity1, boxed_identity2);
    }
}
