use std::{mem, ptr};

use boxed::{Any, ConstructableFrom, Gc, Header};

/// Represents a garbage collected Heap
///
/// This has a gross pointer-based representation to allow use as a bump allocator from generated
/// native code.
#[repr(C)]
pub struct Heap {
    start: *mut Any,
    end: *const Any,
    backing_vec: Vec<Any>,
}

impl Heap {
    pub fn with_capacity(count: usize) -> Heap {
        let mut backing_vec = Vec::with_capacity(count);
        let start: *mut Any = backing_vec.as_mut_ptr();

        Heap {
            start,
            end: unsafe { start.offset(count as isize) },
            backing_vec,
        }
    }

    pub fn new_box<B, V>(&mut self, value: V) -> Gc<B>
    where
        B: ConstructableFrom<V>,
    {
        let heap_size = B::heap_size_for_value(&value);
        let needed_cells = heap_size.cell_count();

        let insert_at = self.start;
        let new_start = unsafe { self.start.offset(needed_cells as isize) };

        if (new_start as *const Any) > self.end {
            unimplemented!("Allocate more memory")
        } else {
            self.start = new_start;
        }

        let header = Header {
            type_tag: B::TYPE_TAG,
            alloc_type: heap_size.to_alloc_type(),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_alloc() {
        use boxed::Str;

        let mut heap = Heap::with_capacity(5);

        let string1 = heap.new_box::<Str, _>("HELLO");
        let string2 = heap.new_box::<Str, _>("WORLD");

        assert_eq!("HELLO", string1.as_str());
        assert_eq!("WORLD", string2.as_str());
    }
}
