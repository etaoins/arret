use std::{cmp, mem, ptr};

use boxed::refs::Gc;
use boxed::{Any, ConstructableFrom, Header};

/// Represents an allocated segement of garbage collected memory
///
/// This has a gross pointer-based representation to allow use as a bump allocator from generated
/// native code.
#[repr(C)]
pub struct Segment {
    start: *mut Any,
    end: *const Any,
    backing_vec: Vec<Any>,
}

#[repr(C)]
pub struct Heap {
    pub current_segment: Segment,
    pub full_segments: Vec<Segment>,
}

impl Segment {
    /// Creates a new segment with capacity for `count` cells
    fn with_capacity(count: usize) -> Segment {
        let mut backing_vec = Vec::with_capacity(count);
        let start: *mut Any = backing_vec.as_mut_ptr();

        Segment {
            start,
            end: unsafe { start.offset(count as isize) },
            backing_vec,
        }
    }

    /// Returns contiguous memory for holding `count` cells
    ///
    /// If the segment is full this will return None
    fn alloc_cells(&mut self, count: usize) -> Option<*mut Any> {
        let current_start = self.start;
        let new_start = unsafe { self.start.offset(count as isize) };

        if (new_start as *const Any) > self.end {
            None
        } else {
            self.start = new_start;
            Some(current_start)
        }
    }
}

impl Heap {
    /// Capacity of the initial segment and all overflow segments
    const DEFAULT_SEGMENT_CAPACITY: usize = 1024;

    pub fn new() -> Heap {
        Self::with_capacity(Self::DEFAULT_SEGMENT_CAPACITY)
    }

    pub fn with_capacity(count: usize) -> Heap {
        Heap {
            current_segment: Segment::with_capacity(count),
            full_segments: vec![],
        }
    }

    fn alloc_cells(&mut self, count: usize) -> *mut Any {
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

    pub fn new_box<B, V>(&mut self, value: V) -> Gc<B>
    where
        B: ConstructableFrom<V>,
    {
        let heap_size = B::size_for_value(&value);
        let needed_cells = heap_size.cell_count();

        let insert_at = self.alloc_cells(needed_cells);

        let header = Header {
            type_tag: B::TYPE_TAG,
            alloc_type: heap_size.to_heap_alloc_type(),
        };

        let stack_box = B::new_with_header(value, header);

        unsafe {
            ptr::copy_nonoverlapping(&stack_box, insert_at as *mut B, needed_cells);
        }

        // Make sure we don't drop the stack version
        mem::forget(stack_box);
        unsafe { Gc::new(insert_at as *const B) }
    }
}

impl Default for Heap {
    fn default() -> Heap {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_alloc() {
        use boxed::Str;

        let mut heap = Heap::with_capacity(1);

        let string1 = heap.new_box::<Str, _>("HELLO");
        let string2 = heap.new_box::<Str, _>("WORLD");

        assert_eq!("HELLO", string1.as_str());
        assert_eq!("WORLD", string2.as_str());
    }
}
