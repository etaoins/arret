pub mod collect;

use std::{cmp, mem, ptr};

use crate::boxed::refs::Gc;
use crate::boxed::{AllocType, Any, Boxed};
use crate::intern::Interner;

/// Allocated segment of garbage collected memory
///
/// This has a gross pointer-based representation to allow use as a bump allocator from generated
/// native code.
#[repr(C)]
pub struct Segment {
    next: *mut Any,
    end: *const Any,
    backing_vec: Vec<Any>,
}

/// Heap of garbage collected boxes
#[repr(C)]
pub struct Heap {
    current_segment: Segment,
    full_segments: Vec<Segment>,
    interner: Interner,
    len_at_last_gc: usize,
}

impl Segment {
    /// Creates a new segment with capacity for `count` cells
    fn with_capacity(count: usize) -> Segment {
        let mut backing_vec = Vec::with_capacity(count);
        let next: *mut Any = backing_vec.as_mut_ptr();

        Segment {
            next,
            end: unsafe { next.add(count) },
            backing_vec,
        }
    }

    /// Returns contiguous memory for holding `count` cells
    ///
    /// If the segment is full this will return `None`
    fn alloc_cells(&mut self, count: usize) -> Option<*mut Any> {
        let current_next = self.next;
        let new_next = unsafe { self.next.add(count) };

        if (new_next as *const Any) > self.end {
            None
        } else {
            self.next = new_next;
            Some(current_next)
        }
    }

    /// Returns the number of allocated cells
    fn len(&self) -> usize {
        // TODO: Replace with `offset_from` once its stable
        (self.next as usize - self.backing_vec.as_ptr() as usize) / mem::size_of::<Any>()
    }
}

impl Drop for Segment {
    fn drop(&mut self) {
        let mut current = self.backing_vec.as_mut_ptr();
        while current < self.next as *mut Any {
            unsafe {
                match (*current).header.alloc_type {
                    AllocType::Heap16 | AllocType::Heap32 => ptr::drop_in_place(current),
                    AllocType::HeapForward16 | AllocType::HeapForward32 => {}
                    AllocType::Const | AllocType::Stack => {
                        unreachable!("Unexpected alloc type in heap")
                    }
                }

                match (*current).header.alloc_type {
                    AllocType::Heap16 | AllocType::HeapForward16 => {
                        current = current.add(1);
                    }
                    AllocType::Heap32 | AllocType::HeapForward32 => {
                        current = current.add(2);
                    }
                    AllocType::Const | AllocType::Stack => {
                        unreachable!("Unexpected alloc type in heap")
                    }
                }
            }
        }
    }
}

impl Heap {
    /// Capacity of the initial segment and all overflow segments
    const DEFAULT_SEGMENT_CAPACITY: usize = 1024;

    /// Default capacity of the heap
    const DEFAULT_CAPACITY: usize = Self::DEFAULT_SEGMENT_CAPACITY;

    /// Returns an empty heap with a default capacity
    pub fn empty() -> Heap {
        Self::new(Interner::new(), Self::DEFAULT_CAPACITY)
    }

    /// Returns a new hep with the given symbol interner and capacity
    pub fn new(interner: Interner, count: usize) -> Heap {
        Heap {
            current_segment: Segment::with_capacity(count),
            full_segments: vec![],
            interner,
            len_at_last_gc: 0,
        }
    }

    /// Hints if this heap should be garbage collected
    ///
    /// This is a heuristic based on the number of allocations since the last GC cycle.
    pub fn should_collect(&self) -> bool {
        let maximum_len = std::cmp::max(Self::DEFAULT_SEGMENT_CAPACITY, self.len_at_last_gc) * 2;
        self.len() > maximum_len
    }

    fn save_len_at_gc(&mut self) {
        self.len_at_last_gc = self.len();
    }

    /// Allocates space for contiguous `count` cells
    pub fn alloc_cells(&mut self, count: usize) -> *mut Any {
        if let Some(alloc) = self.current_segment.alloc_cells(count) {
            return alloc;
        }

        // Make sure we allocate enough to satisfy the request
        let capacity = cmp::max(count, Self::DEFAULT_SEGMENT_CAPACITY);

        // Build a new segment and allocate from it
        let mut new_segment = Segment::with_capacity(capacity);
        let alloc = new_segment.alloc_cells(count).unwrap();

        // Switch the segment and track the old one for finalisation
        let previous_segment = mem::replace(&mut self.current_segment, new_segment);
        self.full_segments.push(previous_segment);

        alloc
    }

    /// Returns the symbol interner associated with this heap
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Returns a mutable reference to the symbol interner associated with this heap
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    /// Returns the number of allocated cells
    pub fn len(&self) -> usize {
        let full_len: usize = self.full_segments.iter().map(Segment::len).sum();
        self.current_segment.len() + full_len
    }

    /// Returns true if the heap contains no boxes
    pub fn is_empty(&self) -> bool {
        self.current_segment.len() == 0 && self.full_segments.is_empty()
    }

    /// Places a new boxed value on the heap
    pub fn place_box<T: Boxed>(&mut self, boxed: T) -> Gc<T> {
        let heap_size = boxed
            .header()
            .alloc_type()
            .to_heap_box_size()
            .expect("non-heap alloc type");

        let needed_cells = heap_size.cell_count();

        let insert_at = self.alloc_cells(needed_cells);

        unsafe {
            ptr::copy_nonoverlapping(&boxed as *const T as *const Any, insert_at, needed_cells);
        }

        // Make sure we don't drop the stack version
        mem::forget(boxed);
        unsafe { Gc::new(insert_at as *const T) }
    }
}

/// Object that can be used as a heap
pub trait AsHeap {
    /// Returns this object as a heap
    fn as_heap(&self) -> &Heap;

    /// Returns this object as a mutable heap
    fn as_heap_mut(&mut self) -> &mut Heap;
}

impl AsHeap for Heap {
    fn as_heap(&self) -> &Heap {
        self
    }

    fn as_heap_mut(&mut self) -> &mut Heap {
        self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_alloc() {
        use crate::boxed::Str;

        let mut heap = Heap::new(Interner::new(), 1);

        let string1 = Str::new(&mut heap, "HELLO");
        let string2 = Str::new(&mut heap, "WORLD");

        assert_eq!("HELLO", string1.as_str());
        assert_eq!("WORLD", string2.as_str());
    }
}
