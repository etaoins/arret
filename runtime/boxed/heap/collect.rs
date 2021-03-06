//! Functionality for garbage collecting heaps
//!
//! This is a basic tracing, moving garbage collector. It doesn't support generations or concurrent
//! collection. Every collection starts with a strong pass followed by an optional weak pass.

use std::ptr;

use crate::boxed;
use crate::boxed::heap::Heap;
use crate::boxed::refs::Gc;
use crate::boxed::{AllocType, BoxSize, Boxed, TypeTag};
use crate::intern::InternedSym;

#[repr(C, align(16))]
struct ForwardingCell {
    header: boxed::Header,
    new_location: Gc<boxed::Any>,
}

/// Strong pass from an old [`Heap`] in to a new [`Heap`]
///
/// [`visit_box`](StrongPass::visit_box) should be called for each GC root that needs to be moved to
/// the new heap. Once all roots have been visited [`into_new_heap`](StrongPass::into_new_heap) will
/// return the new [`Heap`] or [`into_weak_pass`](StrongPass::into_weak_pass) will start an optional
/// weak pass.
pub struct StrongPass {
    old_heap: Heap,
    new_heap: Heap,
}

impl StrongPass {
    /// Consumes an existing heap to begin a garbage collection pass
    pub fn new(old_heap: Heap) -> StrongPass {
        let type_info = old_heap.type_info().clone_for_collect_garbage();

        StrongPass {
            old_heap,
            new_heap: Heap::new(type_info, Heap::DEFAULT_CAPACITY),
        }
    }

    /// Continues as a weak reference pass
    pub fn into_weak_pass(self) -> WeakPass {
        WeakPass {
            _old_heap: self.old_heap,
            new_heap: self.new_heap,
        }
    }

    /// Finishes garbage collection by returning the new heap
    pub fn into_new_heap(self) -> Heap {
        let mut new_heap = self.new_heap;
        new_heap.save_len_at_gc();
        new_heap
    }

    /// Visits a garbage collected box as a strong root
    pub fn visit_box<T: Boxed>(&mut self, box_ref: &mut Gc<T>) {
        let any_box_ref = unsafe { &mut *(box_ref as *mut _ as *mut Gc<boxed::Any>) };
        Self::visit_any_box(&self.old_heap, &mut self.new_heap, any_box_ref);
    }

    fn move_box_to_new_heap(new_heap: &mut Heap, box_ref: &mut Gc<boxed::Any>, size: BoxSize) {
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
            header: boxed::Header {
                // This is arbitrary but could be useful for debugging
                type_tag: box_ref.header.type_tag,
                alloc_type: forward_alloc_type,
            },
            new_location: unsafe { Gc::new(dest_location) },
        };

        // Overwrite the previous box location
        unsafe {
            ptr::copy_nonoverlapping(
                &forwarding_cell as *const ForwardingCell as *const boxed::Any,
                box_ref.as_ptr() as *mut boxed::Any,
                1,
            );
        }

        // Update the box_ref
        *box_ref = unsafe { Gc::new(dest_location) };
    }

    /// Re-interns the symbol on a new heap
    fn visit_interned_sym(old_heap: &Heap, new_heap: &mut Heap, interned_sym: &mut InternedSym) {
        let old_interner = old_heap.type_info().interner();
        let new_interner = new_heap.type_info_mut().interner_mut();

        let sym_name = old_interner.unintern(interned_sym);
        *interned_sym = new_interner.intern(sym_name);
    }

    fn visit_any_box(old_heap: &Heap, new_heap: &mut Heap, mut box_ref: &mut Gc<boxed::Any>) {
        // This loop is used for ad-hoc tail recursion when visiting Pairs and FunThunks
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
                    Self::move_box_to_new_heap(new_heap, box_ref, BoxSize::Size16);
                }
                AllocType::Heap32 => {
                    Self::move_box_to_new_heap(new_heap, box_ref, BoxSize::Size32);
                }
                AllocType::Stack => {
                    // Stack boxes cannot move but they may point to heap boxes
                }
            }

            match box_ref.header.type_tag {
                TypeTag::Sym => {
                    let sym_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut boxed::Sym) };
                    Self::visit_interned_sym(old_heap, new_heap, sym_ref.interned_mut());
                }
                TypeTag::Pair => {
                    let pair_ref =
                        unsafe { &mut *(box_ref.as_mut_ptr() as *mut boxed::Pair<boxed::Any>) };

                    Self::visit_any_box(old_heap, new_heap, &mut pair_ref.head);

                    // Start again with the tail of the list
                    box_ref = unsafe {
                        &mut *(&mut pair_ref.rest as *mut Gc<boxed::List<boxed::Any>>
                            as *mut Gc<boxed::Any>)
                    };
                    continue;
                }
                TypeTag::Vector => {
                    let vec_ref =
                        unsafe { &mut *(box_ref.as_mut_ptr() as *mut boxed::Vector<boxed::Any>) };

                    vec_ref.visit_mut_elements(&mut |elem_ref| {
                        Self::visit_any_box(old_heap, new_heap, elem_ref);
                    });
                }
                TypeTag::FunThunk => {
                    let fun_thunk_ref =
                        unsafe { &mut *(box_ref.as_mut_ptr() as *mut boxed::FunThunk) };

                    // Start again with the captures
                    box_ref = unsafe { &mut *(&mut fun_thunk_ref.captures as *mut Gc<boxed::Any>) };
                    continue;
                }
                TypeTag::Record => {
                    use crate::boxed::types::field_value::FieldGcRef;

                    let record_ref = unsafe { &mut *(box_ref.as_mut_ptr() as *mut boxed::Record) };
                    for field_gc_ref in record_ref.field_gc_refs(old_heap) {
                        match field_gc_ref {
                            FieldGcRef::Boxed(field_box_ref) => {
                                Self::visit_any_box(old_heap, new_heap, field_box_ref);
                            }
                            FieldGcRef::InternedSym(interned_sym) => {
                                Self::visit_interned_sym(old_heap, new_heap, interned_sym);
                            }
                        }
                    }
                }
                _ => {}
            }

            return;
        }
    }
}

/// Weak pass of a collection to a new [`Heap`]
///
/// This will return the location of cells that have been moved to the new heap or [`None`] for
/// cells that were not visited during the strong pass.
pub struct WeakPass {
    // We need the old heap to remain allocated so we can follow pointers for old cells
    _old_heap: Heap,
    new_heap: Heap,
}

impl WeakPass {
    /// Finishes garbage collection by returning the new [`Heap`]
    pub fn into_new_heap(self) -> Heap {
        let mut new_heap = self.new_heap;
        new_heap.save_len_at_gc();
        new_heap
    }

    /// Visits a garbage collected box
    ///
    /// If the box was moved during the strong pass its new location will be returned. Otherwise,
    /// [`None`] will be returned.
    pub fn new_heap_ref_for<T: Boxed>(&self, boxed: Gc<T>) -> Option<Gc<T>> {
        let any_boxed = unsafe { boxed.cast::<boxed::Any>() };
        self.new_heap_any_ref_for(any_boxed)
            .map(|new_box| unsafe { new_box.cast::<T>() })
    }

    fn new_heap_any_ref_for(&self, box_ref: Gc<boxed::Any>) -> Option<Gc<boxed::Any>> {
        match box_ref.header.alloc_type {
            AllocType::Const | AllocType::Stack => {
                // These aren't managed by the GC; their pointer remains valid
                Some(box_ref)
            }
            AllocType::HeapForward16 | AllocType::HeapForward32 => {
                // This has already been moved to a new location
                let forwarding_cell = unsafe { &*(box_ref.as_ptr() as *const ForwardingCell) };
                Some(forwarding_cell.new_location)
            }
            AllocType::Heap16 | AllocType::Heap32 => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::{Int, List, Str};

    #[test]
    fn simple_collect() {
        let mut old_heap = Heap::empty();

        let mut hello = Str::new(&mut old_heap, "HELLO");
        let mut world = Str::new(&mut old_heap, "WORLD");

        assert_eq!("HELLO", hello.as_str());
        assert_eq!("WORLD", world.as_str());
        assert_eq!(2, old_heap.len());

        // Root everything
        let mut all_strong = StrongPass::new(old_heap);
        all_strong.visit_box(&mut hello);
        all_strong.visit_box(&mut world);

        let all_heap = all_strong.into_new_heap();
        assert_eq!("HELLO", hello.as_str());
        assert_eq!("WORLD", world.as_str());
        assert_eq!(2, all_heap.len());

        // Take aliases to hello and world to simulate weak reference
        let hello_alias = hello;
        let world_alias = world;

        // Root just one string
        let mut one_strong = StrongPass::new(all_heap);
        one_strong.visit_box(&mut hello);

        // Start a weak pass
        let one_weak = one_strong.into_weak_pass();
        assert!(one_weak.new_heap_ref_for(hello_alias).is_some());
        assert!(one_weak.new_heap_ref_for(world_alias).is_none());

        let one_heap = one_weak.into_new_heap();
        assert_eq!("HELLO", hello.as_str());
        assert_eq!(1, one_heap.len());

        // Root nothing
        let zero_heap = StrongPass::new(one_heap).into_new_heap();
        assert_eq!(0, zero_heap.len());
    }

    #[test]
    fn sym_collect() {
        use crate::boxed::Sym;

        let mut old_heap = Heap::empty();

        let inline_name = "Hello";
        let indexed_name = "This is too long; it will be indexed to the heap's intern table";

        let mut inline = Sym::new(&mut old_heap, inline_name);
        let mut indexed = Sym::new(&mut old_heap, indexed_name);
        assert_eq!(2, old_heap.len());

        let mut all_strong = StrongPass::new(old_heap);
        all_strong.visit_box(&mut inline);
        all_strong.visit_box(&mut indexed);

        let all_heap = all_strong.into_new_heap();

        assert_eq!(inline_name, inline.name(&all_heap));
        assert_eq!(indexed_name, indexed.name(&all_heap));
        assert_eq!(2, all_heap.len());
    }

    #[test]
    fn list_collect() {
        use std::mem;

        // Three 1 cell integers + three pairs
        const PAIR_CELLS: usize =
            mem::size_of::<boxed::Pair<boxed::Any>>() / mem::size_of::<boxed::Any>();

        const EXPECTED_HEAP_SIZE: usize = 3 + (3 * PAIR_CELLS);

        let mut old_heap = Heap::empty();

        let mut boxed_list = List::from_values(&mut old_heap, [1, 2, 3].iter().cloned(), Int::new);
        assert_eq!(EXPECTED_HEAP_SIZE, old_heap.len());

        assert_eq!(3, boxed_list.len());

        let mut all_strong = StrongPass::new(old_heap);
        all_strong.visit_box(&mut boxed_list);

        let all_heap = all_strong.into_new_heap();
        assert_eq!(3, boxed_list.len());
        assert_eq!(EXPECTED_HEAP_SIZE, all_heap.len());

        let mut boxed_list_iter = boxed_list.iter();
        for expected_num in &[1, 2, 3] {
            if let Some(boxed_int) = boxed_list_iter.next() {
                assert_eq!(*expected_num, boxed_int.value());
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
            let mut old_heap = Heap::empty();
            let mut boxed_vec =
                boxed::Vector::from_values(&mut old_heap, test_content.iter().cloned(), Int::new);

            let mut all_strong = StrongPass::new(old_heap);
            all_strong.visit_box(&mut boxed_vec);

            // Need to give this a name so it doesn't Drop
            let _all_heap = all_strong.into_new_heap();

            let mut boxed_list_iter = boxed_vec.iter();
            assert_eq!(test_content.len(), boxed_list_iter.len());

            for expected_num in test_content {
                if let Some(boxed_int) = boxed_list_iter.next() {
                    assert_eq!(*expected_num, boxed_int.value());
                } else {
                    panic!("Iterator unexpectedly ended");
                }
            }
        }
    }
}
