use crate::boxed::heap::Heap;
use crate::boxed::prelude::*;

pub struct Task {
    heap: Heap,
}

impl Task {
    pub fn new() -> Task {
        Task {
            heap: Heap::with_capacity(32),
        }
    }

    pub fn heap(&mut self) -> &mut Heap {
        &mut self.heap
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
