#![warn(missing_docs)]

//! Isolated tasks of execution

use std::panic;

use crate::binding::Never;
use crate::boxed::prelude::*;
use crate::boxed::Heap;
use crate::intern::{GlobalName, Interner};

/// Isolated task of execution
///
/// All Arret and RFI code must run inside a task. It provides a dedicated garbage collected
/// [`Heap`] as well as an isolation boundary against panics. A task is inherently single threaded;
/// it's not possible for one task to be executing on multiple threads at the same time.
pub struct Task {
    heap: Heap,
}

impl Task {
    const DEFAULT_CAPACITY: usize = 32;

    /// Creates a new empty task
    pub fn new() -> Task {
        Task {
            heap: Heap::new(Interner::new(), Self::DEFAULT_CAPACITY),
        }
    }

    pub(crate) fn with_global_interned_names(names: *const GlobalName) -> Task {
        Task {
            heap: Heap::new(Interner::with_global_names(names), Self::DEFAULT_CAPACITY),
        }
    }

    /// Returns this task's dedicated heap
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    /// Returns a mutable reference to this task's dedicated heap
    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    /// Panics the current task
    ///
    /// This destroys the current task and invokes any cleanup required.
    pub fn panic(&mut self, message: String) -> Never {
        // Using `resume_unwind` accomplishes two things:
        //
        // 1. Avoids printing the panic info to stderr as this is an "intentional" panic
        // 2. Skips incrementing our panic count. This is important for compile time evaluation of
        //    panics. The compiler and stdlib have a different panic count due to being in separate
        //    binaries. With a normal `panic!` the panic count will be increment on the stdlib and
        //    decremented in the compiler. On the second panic the stdlib thinks it's already
        //    panicking and aborts. This is a hacky workaround.
        //
        // TODO: Fix panics uniformly and remove this method. If we panic inside e.g. Rust stdlib
        // we won't follow this path.
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
