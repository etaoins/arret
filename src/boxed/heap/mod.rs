mod collect;

use std::{cmp, mem, ptr};

use boxed::refs::Gc;
use boxed::{AllocType, Any, ConstructableFrom};

/// Represents an allocated segement of garbage collected memory
///
/// This has a gross pointer-based representation to allow use as a bump allocator from generated
/// native code.
#[repr(C)]
pub struct Segment {
    next: *mut Any,
    end: *const Any,
    backing_vec: Vec<Any>,
}

#[repr(C)]
pub struct Heap {
    current_segment: Segment,
    full_segments: Vec<Segment>,
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
    /// If the segment is full this will return None
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

    pub fn new() -> Heap {
        Self::with_capacity(Self::DEFAULT_SEGMENT_CAPACITY)
    }

    pub fn with_capacity(count: usize) -> Heap {
        Heap {
            current_segment: Segment::with_capacity(count),
            full_segments: vec![],
        }
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

    /// Returns the number of allocated cells
    fn len(&self) -> usize {
        let full_len: usize = self.full_segments.iter().map(|s| s.len()).sum();
        self.current_segment.len() + full_len
    }

    /// Constructs a new boxed value on the heap
    pub fn new_box<B, V>(&mut self, value: V) -> Gc<B>
    where
        B: ConstructableFrom<V>,
    {
        let heap_size = B::size_for_value(&value);
        let needed_cells = heap_size.cell_count();

        let insert_at = self.alloc_cells(needed_cells);
        let alloc_type = heap_size.to_heap_alloc_type();

        let stack_box = B::new_with_alloc_type(value, alloc_type);

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

        let string1 = Str::new(&mut heap, "HELLO");
        let string2 = Str::new(&mut heap, "WORLD");

        assert_eq!("HELLO", string1.as_str());
        assert_eq!("WORLD", string2.as_str());
    }
}
