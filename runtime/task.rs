use std::panic;

use crate::binding::Never;
use crate::boxed::prelude::*;
use crate::boxed::Heap;
use crate::intern::Interner;

pub struct Task {
    heap: Heap,
}

impl Task {
    pub fn new() -> Task {
        Task {
            heap: Heap::new(Interner::new(), 32),
        }
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    /// Panics the current task
    ///
    /// This destroys the current task and invokes any cleanup required
    pub fn panic(&mut self, message: String) -> Never {
        // Using `resume_unwind` accomplishes two things:
        //
        // 1. Avoids printing the panic info to stderr as this is an "intentional" panic
        // 2. Skips incrementing our panic count. This is important for compile time evaluation of
        //    panics. The compiler and stdlib have a different panic count due to being in separate
        //    binaries. With a normal `panic!` the panic count will be increment on the stdlib and
        //    decremented in the compiler. On the second panic the stdlib thinks it's already
        //    panicking and aborts. This is a hacky workaround.
        panic::resume_unwind(Box::new(message));
    }
}

impl Default for Task {
    fn default() -> Task {
        Task::new()
    }
}

impl AsHeap for Task {
    fn as_heap(&self) -> &Heap {
        &self.heap
    }

    fn as_heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }
}
