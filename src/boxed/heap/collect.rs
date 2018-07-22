use std::ptr;

use boxed::heap::Heap;
use boxed::refs::Gc;
use boxed::{AllocType, Any, BoxSize, Header, List, Pair, Sym, TypeTag, Vector};

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

fn visit_box(mut box_ref: &mut Gc<Any>, old_heap: &Heap, new_heap: &mut Heap) {
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
            TypeTag::Sym => {
                let mut sym_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut Sym) };

                // If this symbol is heap indexed we need to reintern it on the new heap
                let new_interned_name = {
                    let sym_name = sym_ref.name(&old_heap.interner);
                    new_heap.interner.intern(sym_name)
                };
                sym_ref.interned = new_interned_name;
            }
            TypeTag::TopPair => {
                let mut pair_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut Pair<Any>) };

                visit_box(&mut pair_ref.head, old_heap, new_heap);

                // Start again with the tail of the list
                box_ref =
                    unsafe { &mut *(&mut pair_ref.rest as *mut Gc<List<Any>> as *mut Gc<Any>) };
                continue;
            }
            TypeTag::TopVector => {
                let mut vec_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut Vector<Any>) };

                for elem_ref in vec_ref.values_mut() {
                    visit_box(elem_ref, old_heap, new_heap);
                }
            }
            _ => {}
        }

        return;
    }
}

// We could take `Heap` by reference but after we collect the old heap is unusable. Taking it by
// value lets us use Rust's ownership system to express that.
#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn collect_roots(old_heap: Heap, roots: Vec<&mut Gc<Any>>) -> Heap {
    let mut new_heap = Heap::new();

    for root in roots {
        visit_box(root, &old_heap, &mut new_heap);
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
                collect_roots(old_heap, all_roots)
            };

            assert_eq!("HELLO", hello.cast::<Str>().as_str());
            assert_eq!("WORLD", world.cast::<Str>().as_str());
            assert_eq!(2, all_heap.len());

            // Root just one string
            let one_heap = {
                let one_roots = vec![&mut hello];
                collect_roots(all_heap, one_roots)
            };

            assert_eq!("HELLO", hello.cast::<Str>().as_str());
            assert_eq!(1, one_heap.len());

            // Root nothing
            let zero_heap = collect_roots(one_heap, vec![]);
            assert_eq!(0, zero_heap.len());
        }
    }

    #[test]
    fn sym_collect() {
        use boxed::{ConstructableFrom, Sym};

        let mut old_heap = Heap::new();

        unsafe {
            let inline_name = "Hello";
            let indexed_name = "This is too long; it will be indexed to the heap's intern table";

            let mut inline = Sym::new(&mut old_heap, inline_name).cast::<Any>();
            let mut indexed = Sym::new(&mut old_heap, indexed_name).cast::<Any>();
            assert_eq!(2, old_heap.len());

            let new_heap = {
                let all_roots = vec![&mut inline, &mut indexed];
                collect_roots(old_heap, all_roots)
            };

            assert_eq!(inline_name, inline.cast::<Sym>().name(&new_heap.interner));
            assert_eq!(indexed_name, indexed.cast::<Sym>().name(&new_heap.interner));
            assert_eq!(2, new_heap.len());
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

        let new_heap = collect_roots(
            heap,
            vec![unsafe { &mut *(&mut boxed_list as *mut Gc<List<Int>> as *mut Gc<Any>) }],
        );

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
        // Try empty, 1 cell inline, 2 cell inline, and large vectors
        let test_contents: [&[i64]; 4] = [&[], &[1], &[1, 2, 3], &[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]];

        for &test_content in &test_contents {
            let mut heap = Heap::new();

            let boxed_ints = test_content
                .iter()
                .map(|num| Int::new(&mut heap, *num))
                .collect::<Vec<Gc<Int>>>();
            assert_eq!(test_content.len(), heap.len());
            let mut boxed_vector = Vector::new(&mut heap, boxed_ints.as_slice());

            let _new_heap = collect_roots(
                heap,
                vec![unsafe { &mut *(&mut boxed_vector as *mut Gc<Vector<Int>> as *mut Gc<Any>) }],
            );

            let mut boxed_list_iter = boxed_vector.values().iter();
            assert_eq!(test_content.len(), boxed_list_iter.len());

            for expected_num in test_content {
                if let Some(boxed_int) = boxed_list_iter.next() {
                    assert_eq!(*expected_num, boxed_int.value);
                } else {
                    panic!("Iterator unexpectedly ended");
                }
            }
        }
    }
}
