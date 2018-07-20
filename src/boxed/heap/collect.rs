use std::ptr;

use boxed::heap::Heap;
use boxed::refs::Gc;
use boxed::{AllocType, Any, BoxSize, Header, List, Pair, TypeTag, Vector};

#[repr(C, align(16))]
pub struct ForwardingCell {
    pub header: Header,
    pub new_location: Gc<Any>,
}

fn move_box_to_new_heap(box_ref: &mut Gc<Any>, new_heap: &mut Heap, size: BoxSize) {
    // Allocate and copy to the new heap
    let dest_location = new_heap.alloc_cells(size.cell_count());
    unsafe {
        ptr::copy_nonoverlapping(box_ref.as_ptr(), dest_location, size.cell_count());
    }

    let forward_alloc_type = match size {
        BoxSize::Size16 => AllocType::HeapForward16,
        BoxSize::Size32 => AllocType::HeapForward32,
    };

    // Create a forwarding cell
    let forwarding_cell = ForwardingCell {
        header: Header {
            // This is arbitrary but could be useful for debugging
            type_tag: box_ref.header.type_tag,
            alloc_type: forward_alloc_type,
        },
        new_location: unsafe { Gc::new(dest_location) },
    };

    // Overwrite the previous box location
    unsafe {
        ptr::copy_nonoverlapping(
            &forwarding_cell as *const ForwardingCell as *const Any,
            box_ref.as_ptr() as *mut Any,
            1,
        );
    }

    // Update the box_ref
    *box_ref = unsafe { Gc::new(dest_location) };
}

fn visit_box(mut box_ref: &mut Gc<Any>, new_heap: &mut Heap) {
    // This loop is used for ad-hoc tail recursion when visiting Pairs
    // Everything else will return at the bottom of the loop
    loop {
        match box_ref.header.alloc_type {
            AllocType::Const => {
                // Return when encountering a const box; they cannot move and cannot refer to the heap
                return;
            }
            AllocType::HeapForward16 | AllocType::HeapForward32 => {
                // This has already been moved to a new location
                let forwarding_cell = unsafe { &*(box_ref.as_ptr() as *const ForwardingCell) };
                *box_ref = forwarding_cell.new_location;
                return;
            }
            AllocType::Heap16 => {
                move_box_to_new_heap(box_ref, new_heap, BoxSize::Size16);
            }
            AllocType::Heap32 => {
                move_box_to_new_heap(box_ref, new_heap, BoxSize::Size32);
            }
            AllocType::Stack => {
                // Stack boxes cannot move but they may point to heap boxes
            }
        }

        match box_ref.header.type_tag {
            TypeTag::TopPair => {
                let mut pair_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut Pair<Any>) };

                visit_box(&mut pair_ref.head, new_heap);

                // Start again with the tail of the list
                box_ref =
                    unsafe { &mut *(&mut pair_ref.rest as *mut Gc<List<Any>> as *mut Gc<Any>) };
                continue;
            }
            TypeTag::TopVector => {
                let mut vec_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut Vector<Any>) };

                for elem_ref in &mut vec_ref.values {
                    visit_box(elem_ref, new_heap);
                }
            }
            _ => {}
        }

        return;
    }
}

pub fn collect_roots(roots: Vec<&mut Gc<Any>>) -> Heap {
    let mut new_heap = Heap::new();

    for root in roots {
        visit_box(root, &mut new_heap);
    }

    new_heap
}

#[cfg(test)]
mod test {
    use super::*;
    use boxed::prelude::*;
    use boxed::{Int, List};

    #[test]
    fn simple_collect() {
        use boxed::{ConstructableFrom, Str};

        let mut old_heap = Heap::new();

        unsafe {
            let mut hello = Str::new(&mut old_heap, "HELLO").cast::<Any>();
            let mut world = Str::new(&mut old_heap, "WORLD").cast::<Any>();

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

    #[test]
    fn list_collect() {
        // Three 1 cell integers + three 2 cell pair
        const EXPECTED_HEAP_SIZE: usize = 3 + (3 * 2);
        let mut heap = Heap::new();

        let boxed_ints = [1, 2, 3]
            .iter()
            .map(|num| Int::new(&mut heap, *num))
            .collect::<Vec<Gc<Int>>>();
        assert_eq!(3, heap.len());

        let mut boxed_list = List::new(&mut heap, boxed_ints.into_iter());
        assert_eq!(EXPECTED_HEAP_SIZE, heap.len());

        assert_eq!(3, boxed_list.len());

        let new_heap = collect_roots(vec![unsafe {
            &mut *(&mut boxed_list as *mut Gc<List<Int>> as *mut Gc<Any>)
        }]);

        assert_eq!(3, boxed_list.len());
        assert_eq!(EXPECTED_HEAP_SIZE, new_heap.len());

        let mut boxed_list_iter = boxed_list.iter();
        for expected_num in &[1, 2, 3] {
            if let Some(boxed_int) = boxed_list_iter.next() {
                assert_eq!(*expected_num, boxed_int.value);
            } else {
                panic!("Iterator unexpectedly ended");
            }
        }
    }

    #[test]
    fn vector_collect() {
        // Three 1 cell integers + one 2 cell vector
        const EXPECTED_HEAP_SIZE: usize = 3 + 2;
        let mut heap = Heap::new();

        let boxed_ints = [1, 2, 3]
            .iter()
            .map(|num| Int::new(&mut heap, *num))
            .collect::<Vec<Gc<Int>>>();
        assert_eq!(3, heap.len());

        let mut boxed_vector = Vector::new(&mut heap, boxed_ints.as_slice());
        assert_eq!(EXPECTED_HEAP_SIZE, heap.len());

        let new_heap = collect_roots(vec![unsafe {
            &mut *(&mut boxed_vector as *mut Gc<Vector<Int>> as *mut Gc<Any>)
        }]);

        assert_eq!(EXPECTED_HEAP_SIZE, new_heap.len());

        let mut boxed_list_iter = boxed_vector.values.iter();
        for expected_num in &[1, 2, 3] {
            if let Some(boxed_int) = boxed_list_iter.next() {
                assert_eq!(*expected_num, boxed_int.value);
            } else {
                panic!("Iterator unexpectedly ended");
            }
        }
    }
}
