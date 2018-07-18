use std::ptr;

use boxed::heap::Heap;
use boxed::refs::Gc;
use boxed::{AllocType, Any, Header};

#[repr(C, align(16))]
pub struct ForwardingCell {
    pub header: Header,
    pub new_location: Gc<Any>,
}

fn move_cell_to_new_heap(cell_ref: &mut Gc<Any>, new_heap: &mut Heap, count: usize) {
    // Allocate and copy to the new heap
    let dest_location = new_heap.alloc_cells(count);
    unsafe {
        ptr::copy_nonoverlapping(cell_ref.as_ptr(), dest_location, count);
    }

    // Create a forwarding cell
    let forwarding_cell = ForwardingCell {
        header: Header {
            // This is arbitrary but could be useful for debugging
            type_tag: cell_ref.header.type_tag,
            alloc_type: AllocType::HeapForward,
        },
        new_location: unsafe { Gc::new(dest_location) },
    };

    // Overwrite the previous cell location
    unsafe {
        ptr::copy_nonoverlapping(
            &forwarding_cell as *const ForwardingCell as *const Any,
            cell_ref.as_ptr() as *mut Any,
            1,
        );
    }

    // Update the cell_ref
    *cell_ref = unsafe { Gc::new(dest_location) };
}

fn visit_cell(cell_ref: &mut Gc<Any>, new_heap: &mut Heap) {
    match cell_ref.header.alloc_type {
        AllocType::Const => {
            // Return when encountering a const cell; they cannot move and cannot refer to the heap
            return;
        }
        AllocType::HeapForward => {
            // This has already been moved to a new location
            let forwarding_cell = unsafe { &*(cell_ref.as_ptr() as *const ForwardingCell) };
            *cell_ref = forwarding_cell.new_location;
            return;
        }
        AllocType::Heap16 => {
            move_cell_to_new_heap(cell_ref, new_heap, 1);
        }
        AllocType::Heap32 => {
            move_cell_to_new_heap(cell_ref, new_heap, 2);
        }
        AllocType::Stack => {
            // Stack cells cannot move but they may point to heap cells
        }
    }

    // TODO: Visit nested cell references
}

pub fn collect_roots(roots: Vec<&mut Gc<Any>>) -> Heap {
    let mut new_heap = Heap::new();

    for root in roots {
        visit_cell(root, &mut new_heap);
    }

    new_heap
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_collect() {
        use boxed::Str;

        let mut old_heap = Heap::new();

        unsafe {
            let mut hello = old_heap.new_box::<Str, _>("HELLO").cast::<Any>();
            let mut world = old_heap.new_box::<Str, _>("WORLD").cast::<Any>();

            assert_eq!("HELLO", hello.cast::<Str>().as_str());
            assert_eq!("WORLD", world.cast::<Str>().as_str());
            assert_eq!(2, old_heap.len());

            // Root everything
            let all_heap = {
                let all_roots = vec![&mut hello, &mut world];
                collect_roots(all_roots)
            };

            assert_eq!("HELLO", hello.cast::<Str>().as_str());
            assert_eq!("WORLD", world.cast::<Str>().as_str());
            assert_eq!(2, all_heap.len());

            // Root just one string
            let one_heap = {
                let one_roots = vec![&mut hello];
                collect_roots(one_roots)
            };

            assert_eq!("HELLO", hello.cast::<Str>().as_str());
            assert_eq!(1, one_heap.len());

            // Root nothing
            let zero_heap = collect_roots(vec![]);
            assert_eq!(0, zero_heap.len());
        }
    }

}
