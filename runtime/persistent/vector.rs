// This module unused at the moment
#![allow(dead_code)]

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::{alloc, ptr, sync};

use crate::boxed::refs::Gc;
use crate::boxed::Boxed;

/// Reference count used for global constants created by codegen
const GLOBAL_CONSTANT_REFCOUNT: u64 = std::u64::MAX;

const TRIE_RADIX: u32 = 5;
const NODE_SIZE: usize = 1 << TRIE_RADIX;
const LEVEL_MASK: usize = (1 << TRIE_RADIX) - 1;

pub trait Element: Copy {
    /// A value that's cheap to create without causing undefined behaviour
    fn padding_value() -> Self;
}

impl<T> Element for T
where
    T: Default + Copy,
{
    fn padding_value() -> Self {
        Self::default()
    }
}

impl<T> Element for Gc<T>
where
    T: Boxed,
{
    fn padding_value() -> Self {
        unsafe { Gc::new(ptr::null()) }
    }
}

#[repr(C)]
pub struct Vector<T>
where
    T: Element,
{
    size: u64,
    root: *const Node<T>,
    tail: *const Node<T>,
}

impl<T> Vector<T>
where
    T: Element,
{
    pub fn new(values: impl ExactSizeIterator<Item = T>) -> Self {
        let empty_vec = Vector {
            size: 0,
            root: std::ptr::null(),
            tail: std::ptr::null(),
        };

        empty_vec.extend(values)
    }

    pub fn len(&self) -> usize {
        self.size as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&self, value: T) -> Vector<T> {
        let old_tail_size = self.tail_size();
        let mut new_elements: [T; NODE_SIZE] = [T::padding_value(); NODE_SIZE];

        // Copy the previous leaf elements
        new_elements[..old_tail_size]
            .copy_from_slice(unsafe { &(&*self.tail).elements.leaf[..old_tail_size] });

        // Append the new element
        new_elements[old_tail_size] = value;
        let new_leaf = Node::new_leaf(new_elements);

        if old_tail_size < (NODE_SIZE - 1) {
            // This only modifies the tail
            return {
                Self {
                    size: self.size + 1,
                    root: Node::take_ptr_ref(self.root),
                    tail: new_leaf,
                }
            };
        }

        self.push_leaf(new_leaf, 1)
    }

    /// Pushes a new leaf containing `added_elements` additional elements
    fn push_leaf(&self, leaf: *const Node<T>, added_elements: u64) -> Vector<T> {
        let new_size = self.size + added_elements;
        debug_assert!(new_size & (LEVEL_MASK as u64) == 0);

        if self.root.is_null() {
            // We're the first root node
            return Self {
                size: new_size,
                root: leaf,
                tail: ptr::null(),
            };
        }

        let old_depth = Self::trie_depth(self.trie_size());
        let new_depth = Self::trie_depth(self.trie_size() + NODE_SIZE);

        let new_root = if old_depth == new_depth {
            unsafe { &*self.root }
                .assoc_leaf(old_depth, new_size as usize - 1, leaf)
                .new_subtree
        } else {
            // Need to add a new level
            let mut root_children: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];
            root_children[0] = Node::take_ptr_ref(self.root);
            root_children[1] = Node::new_chain(leaf, old_depth);

            Node::new_branch(root_children)
        };

        Self {
            size: new_size,
            root: new_root,
            tail: ptr::null(),
        }
    }

    pub fn pop(&self) -> Option<(Vector<T>, T)> {
        if self.is_empty() {
            return None;
        }

        let old_tail_size = self.tail_size();
        if old_tail_size > 0 {
            let element = unsafe { (&*self.tail).elements.leaf[old_tail_size - 1] };

            // No change to structure; we just need to change our size
            return Some((
                Self {
                    size: self.size - 1,
                    root: Node::take_ptr_ref(self.root),
                    tail: Node::take_ptr_ref(self.tail),
                },
                element,
            ));
        }

        // We have no tail; we need to take `NODE_SIZE` elements from the trie
        let old_depth = Self::trie_depth(self.trie_size());
        let new_depth = Self::trie_depth(self.trie_size() - NODE_SIZE);

        unsafe {
            let (new_subtree, previous_leaf) = if old_depth == new_depth {
                // Promote the final leaf to to a tail
                let AssocedLeaf {
                    new_subtree,
                    previous_leaf,
                } = (&*self.root).assoc_leaf(new_depth, self.size as usize - 1, ptr::null());

                (new_subtree, previous_leaf)
            } else {
                let new_subtree = Node::take_ptr_ref((&*self.root).elements.branch[0]);
                let previous_leaf = (&*self.root).get_leaf(old_depth, self.size as usize - 1);

                (new_subtree, previous_leaf)
            };

            let element = (&*previous_leaf).elements.leaf[NODE_SIZE - 1];

            Some((
                Self {
                    size: self.size - 1,
                    root: new_subtree,
                    tail: Node::take_ptr_ref(previous_leaf),
                },
                element,
            ))
        }
    }

    pub fn get(&self, index: usize) -> Option<T> {
        if index >= self.len() {
            return None;
        }

        let leaf_node = self.get_leaf(index);
        unsafe { Some((&*leaf_node).elements.leaf[index & LEVEL_MASK]) }
    }

    fn get_leaf(&self, index: usize) -> *const Node<T> {
        if index >= self.tail_offset() {
            self.tail
        } else {
            let depth = Self::trie_depth(self.trie_size());
            unsafe { (&*self.root).get_leaf(depth, index) }
        }
    }

    pub fn assoc(&self, index: usize, value: T) -> Vector<T> {
        if index >= self.len() {
            panic!("element {} of out bounds", index);
        }

        if index >= self.tail_offset() {
            let mut new_elements: [T; NODE_SIZE] = [T::padding_value(); NODE_SIZE];

            // Copy the previous leaf elements
            new_elements[..self.tail_size()]
                .copy_from_slice(unsafe { &(&*self.tail).elements.leaf[..self.tail_size()] });

            // Overwrite the element
            new_elements[index - self.tail_offset()] = value;

            return Self {
                size: self.size,
                root: Node::take_ptr_ref(self.root),
                tail: Node::new_leaf(new_elements),
            };
        }

        let depth = Self::trie_depth(self.trie_size());
        let new_root = unsafe { (&*self.root).assoc_value(depth, index, value) };

        Self {
            size: self.size,
            root: new_root,
            tail: Node::take_ptr_ref(self.tail),
        }
    }

    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = T> + 'a {
        Iter {
            vec: self,
            index: 0,
            current_leaf: self.get_leaf(0),
        }
    }

    pub fn extend(self, mut values: impl ExactSizeIterator<Item = T>) -> Vector<T> {
        // This is a three step process:
        //
        // 1. Push individual values until we have no tail
        // 2. Push whole NODE_SIZE leaves while enough values remain
        // 3. Place the rest of the values in the tail in a single operation
        //
        // We can run out of values at any phase and return the finished vector

        let mut vec_acc = self;

        while (vec_acc.size & LEVEL_MASK as u64) != 0 {
            if let Some(value) = values.next() {
                vec_acc = vec_acc.push(value);
            } else {
                return vec_acc;
            }
        }

        while values.len() >= NODE_SIZE {
            let mut trie_elements: [T; NODE_SIZE] = [T::padding_value(); NODE_SIZE];
            for i in 0..NODE_SIZE {
                trie_elements[i] = values.next().unwrap();
            }

            vec_acc = vec_acc.push_leaf(Node::new_leaf(trie_elements), NODE_SIZE as u64)
        }

        let tail_size = values.len();
        if tail_size > 0 {
            let mut tail_elements: [T; NODE_SIZE] = [T::padding_value(); NODE_SIZE];
            for i in 0..tail_size {
                tail_elements[i] = values.next().unwrap();
            }

            vec_acc.size += tail_size as u64;
            vec_acc.tail = Node::new_leaf(tail_elements);
        }

        vec_acc
    }

    /// Visits each mutable element of the array
    ///
    /// This skips global constants
    pub(crate) fn visit_mut_elements<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut T),
    {
        unsafe {
            if let Some(tail_ref) = (self.tail as *mut Node<T>).as_mut() {
                tail_ref.visit_mut_elements(0, &mut self.tail_size(), visitor);
            }
        }

        unsafe {
            if let Some(root_ref) = (self.root as *mut Node<T>).as_mut() {
                let mut trie_size = self.trie_size();

                root_ref.visit_mut_elements(Self::trie_depth(trie_size), &mut trie_size, visitor);
            }
        }
    }

    /// Size of the trie portion of the `Vector`
    ///
    /// This is always a multiple of `NODE_SIZE`
    fn trie_size(&self) -> usize {
        self.len() - self.tail_size()
    }

    /// Size of the tail portion of the `Vector`
    ///
    /// This is always less than `NODE_SIZE`
    fn tail_size(&self) -> usize {
        self.size as usize % NODE_SIZE
    }

    /// Index of the first element in the tail portion
    fn tail_offset(&self) -> usize {
        self.len() - self.tail_size()
    }

    /// Returns the trie depth for trie of the given size
    fn trie_depth(trie_size: usize) -> u32 {
        if trie_size <= 1 {
            // The root is the only node
            return 0;
        }

        (63 - (trie_size as u64 - 1).leading_zeros()) / TRIE_RADIX
    }
}

impl<T> Drop for Vector<T>
where
    T: Element,
{
    fn drop(&mut self) {
        unsafe {
            Node::release_ptr_ref(self.root, Self::trie_depth(self.trie_size()));
            Node::release_ptr_ref(self.tail, 0);
        }
    }
}

struct AssocedLeaf<T>
where
    T: Element,
{
    new_subtree: *const Node<T>,
    previous_leaf: *const Node<T>,
}

union NodeElements<T>
where
    T: Element,
{
    leaf: [T; NODE_SIZE],
    branch: [*const Node<T>; NODE_SIZE],
}

#[repr(C)]
struct Node<T>
where
    T: Element,
{
    ref_count: AtomicU64,
    elements: NodeElements<T>,
}

impl<T> Node<T>
where
    T: Element,
{
    fn new_leaf(elements: [T; NODE_SIZE]) -> *const Node<T> {
        let layout = alloc::Layout::new::<Self>();

        unsafe {
            let node = alloc::alloc(layout) as *mut Node<T>;

            (*node).ref_count = AtomicU64::new(1);
            (*node).elements.leaf = elements;

            node
        }
    }

    fn new_branch(elements: [*const Node<T>; NODE_SIZE]) -> *const Node<T> {
        let layout = alloc::Layout::new::<Self>();

        debug_assert!(!elements[0].is_null());

        unsafe {
            let node = alloc::alloc(layout) as *mut Node<T>;

            (*node).ref_count = AtomicU64::new(1);
            (*node).elements.branch = elements;

            node
        }
    }

    fn new_chain(leaf_node: *const Node<T>, remaining_depth: u32) -> *const Node<T> {
        if remaining_depth == 0 {
            return leaf_node;
        }

        // Create a one level intermediate node with a single branch
        let rest_tail = Self::new_chain(leaf_node, remaining_depth - 1);

        let mut intermediate_elements = [ptr::null::<Node<T>>(); NODE_SIZE];
        intermediate_elements[0] = rest_tail;

        Self::new_branch(intermediate_elements)
    }

    fn get_value(&self, remaining_depth: u32, index: usize) -> T {
        let leaf_node = self.get_leaf(remaining_depth, index);
        return unsafe { (&*leaf_node).elements.leaf[index & LEVEL_MASK] };
    }

    fn get_leaf(&self, remaining_depth: u32, index: usize) -> *const Node<T> {
        if remaining_depth == 0 {
            return self as *const Node<T>;
        }

        let level_radix = TRIE_RADIX * remaining_depth;
        let branch_index = (index >> level_radix) & LEVEL_MASK;

        unsafe { (&*self.elements.branch[branch_index]).get_leaf(remaining_depth - 1, index) }
    }

    fn assoc_value(&self, remaining_depth: u32, index: usize, value: T) -> *const Node<T> {
        if remaining_depth == 0 {
            // Replace the leaf value
            let mut new_elements = unsafe { self.elements.leaf };
            new_elements[index & LEVEL_MASK] = value;

            return Self::new_leaf(new_elements);
        }

        let level_radix = TRIE_RADIX * remaining_depth;
        let branch_index = (index >> level_radix) & LEVEL_MASK;

        // Replace the branch value
        let new_subtree = unsafe {
            (&*self.elements.branch[branch_index]).assoc_value(remaining_depth - 1, index, value)
        };

        let mut new_elements: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];

        for i in 0..NODE_SIZE {
            unsafe {
                new_elements[i] = if i == branch_index {
                    new_subtree
                } else {
                    Node::take_ptr_ref(&*self.elements.branch[i])
                };
            }
        }

        return Self::new_branch(new_elements);
    }

    fn assoc_leaf(
        &self,
        remaining_depth: u32,
        index: usize,
        leaf: *const Node<T>,
    ) -> AssocedLeaf<T> {
        if remaining_depth == 0 {
            return AssocedLeaf {
                new_subtree: leaf,
                previous_leaf: self as *const _,
            };
        }

        let level_radix = TRIE_RADIX * remaining_depth;
        let branch_index = (index >> level_radix) & LEVEL_MASK;

        // Replace the branch value
        let AssocedLeaf {
            new_subtree,
            previous_leaf,
        } = unsafe {
            match self.elements.branch[branch_index].as_ref() {
                Some(branch) => branch.assoc_leaf(remaining_depth - 1, index, leaf),
                None => AssocedLeaf {
                    new_subtree: Self::new_chain(leaf, remaining_depth - 1),
                    previous_leaf: ptr::null(),
                },
            }
        };

        // Are we empty now?
        if new_subtree.is_null() && branch_index == 0 {
            return AssocedLeaf {
                new_subtree: ptr::null(),
                previous_leaf,
            };
        }

        let mut new_elements: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];

        for i in 0..NODE_SIZE {
            unsafe {
                let new_element = if i == branch_index {
                    new_subtree
                } else {
                    Node::take_ptr_ref(&*self.elements.branch[i])
                };

                new_elements[i] = new_element;
            }
        }

        AssocedLeaf {
            new_subtree: Self::new_branch(new_elements),
            previous_leaf,
        }
    }

    fn is_global_constant(&self) -> bool {
        self.ref_count.load(Ordering::Relaxed) == GLOBAL_CONSTANT_REFCOUNT
    }

    fn take_ptr_ref(self_ptr: *const Node<T>) -> *const Node<T> {
        if let Some(self_ref) = unsafe { self_ptr.as_ref() } {
            if !self_ref.is_global_constant() {
                self_ref.ref_count.fetch_add(1, Ordering::Relaxed);
            }

            self_ptr
        } else {
            ptr::null()
        }
    }

    /// Atomically releases a reference to the node
    unsafe fn release_ptr_ref(self_ptr: *const Node<T>, depth: u32) {
        let self_ref = if let Some(self_ref) = self_ptr.as_ref() {
            self_ref
        } else {
            return;
        };

        if self_ref.is_global_constant() {
            return;
        }

        let should_destroy = self_ref.ref_count.fetch_sub(1, Ordering::Release) == 1;

        if should_destroy {
            sync::atomic::fence(Ordering::Acquire);

            if depth > 0 {
                for i in 0..NODE_SIZE {
                    Self::release_ptr_ref(self_ref.elements.branch[i], depth - 1);
                }
            }

            alloc::dealloc(
                self_ref as *const Self as *mut u8,
                alloc::Layout::new::<Self>(),
            );
        }
    }

    fn visit_mut_elements<F>(
        &mut self,
        remaining_depth: u32,
        remaining_elements: &mut usize,
        visitor: &mut F,
    ) where
        F: FnMut(&mut T),
    {
        if self.is_global_constant() {
            // We're a global constant; skip us
            *remaining_elements = remaining_elements.saturating_sub(NODE_SIZE);
            return;
        }

        if remaining_depth == 0 {
            let leaf_size = std::cmp::min(*remaining_elements, NODE_SIZE);

            for i in 0..leaf_size {
                unsafe {
                    visitor(&mut self.elements.leaf[i]);
                }
            }
            *remaining_elements -= leaf_size;
            return;
        }

        for i in 0..NODE_SIZE {
            unsafe {
                (&mut *(self.elements.branch[i] as *mut Node<T>)).visit_mut_elements(
                    remaining_depth - 1,
                    remaining_elements,
                    visitor,
                );
            }

            if *remaining_elements == 0 {
                return;
            }
        }
    }
}

struct Iter<'a, T>
where
    T: Element,
{
    vec: &'a Vector<T>,
    index: usize,
    current_leaf: *const Node<T>,
}

impl<'a, T> Iterator for Iter<'a, T>
where
    T: Element,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.index >= self.vec.len() {
            return None;
        }

        let item = unsafe { (&*self.current_leaf).elements.leaf[self.index & LEVEL_MASK] };

        self.index = self.index + 1;
        if self.index & LEVEL_MASK == 0 {
            // Lookup the next node
            self.current_leaf = self.vec.get_leaf(self.index);
        }

        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let exact_size = self.vec.size as usize - self.index;

        (exact_size, Some(exact_size))
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> where T: Element {}

impl<T> Clone for Vector<T>
where
    T: Element,
{
    fn clone(&self) -> Self {
        Vector {
            size: self.size,
            root: Node::take_ptr_ref(self.root),
            tail: Node::take_ptr_ref(self.tail),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::iter;

    #[test]
    fn tail_only_vector() {
        let empty_vec = Vector::<i32>::new(iter::empty());

        assert_eq!(0, empty_vec.len());
        assert_eq!(true, empty_vec.is_empty());
        assert_eq!(None, empty_vec.get(0));
        assert!(empty_vec.pop().is_none());

        let one_vec = empty_vec.push(0);

        // Make sure `empty_vec` is still intact
        assert_eq!(0, empty_vec.len());
        assert_eq!(true, empty_vec.is_empty());
        assert_eq!(None, empty_vec.get(0));

        assert_eq!(1, one_vec.len());
        assert_eq!(false, one_vec.is_empty());
        assert_eq!(Some(0), one_vec.get(0));

        // Pop the element off
        let (new_empty_vec, popped_zero) = one_vec.pop().unwrap();

        assert_eq!(0, popped_zero);

        assert_eq!(0, new_empty_vec.len());
        assert_eq!(true, new_empty_vec.is_empty());
        assert_eq!(None, new_empty_vec.get(0));

        // Try modifying the original one item vec
        let mutated_vec = one_vec.assoc(0, 31337);

        assert_eq!(1, mutated_vec.len());
        assert_eq!(false, mutated_vec.is_empty());
        assert_eq!(Some(31337), mutated_vec.get(0));
    }

    #[test]
    fn pushed_one_level_vector() {
        const TEST_LENGTH: usize = 48;

        let mut test_vec = Vector::<usize>::new(iter::empty());

        for i in 0..TEST_LENGTH {
            assert_eq!(i, test_vec.len());
            test_vec = test_vec.push(i);
        }

        // Check the contents manually
        for i in 0..TEST_LENGTH {
            assert_eq!(Some(i), test_vec.get(i));
        }

        // Check the contents with an iterator
        {
            let test_iter = test_vec.iter();
            assert_eq!(TEST_LENGTH, test_iter.len());

            for (actual, expected) in test_vec.iter().enumerate() {
                assert_eq!(expected, actual);
            }
        }

        for i in (0..TEST_LENGTH).rev() {
            let (new_test_vec, element) = test_vec.pop().unwrap();

            assert_eq!(i, new_test_vec.len());
            assert_eq!(i, element);

            test_vec = new_test_vec;
        }

        assert!(test_vec.is_empty());
    }

    #[test]
    fn pushed_two_level_vector() {
        const TEST_LENGTH: usize = 128;

        let mut test_vec = Vector::<usize>::new(iter::empty());

        for i in 0..TEST_LENGTH {
            assert_eq!(i, test_vec.len());
            test_vec = test_vec.push(i);
        }

        // Check the contents manually
        for i in 0..TEST_LENGTH {
            assert_eq!(Some(i), test_vec.get(i));
        }

        // Check the contents with an iterator
        {
            let test_iter = test_vec.iter();
            assert_eq!(TEST_LENGTH, test_iter.len());

            for (actual, expected) in test_vec.iter().enumerate() {
                assert_eq!(expected, actual);
            }
        }

        for i in (0..TEST_LENGTH).rev() {
            let (new_test_vec, element) = test_vec.pop().unwrap();

            assert_eq!(i, new_test_vec.len());
            assert_eq!(i, element);

            test_vec = new_test_vec;
        }

        assert!(test_vec.is_empty());
    }

    #[test]
    fn initialised_three_level_vector() {
        const TEST_LENGTH: usize = 2080;

        let mut test_vec = Vector::<usize>::new(0..TEST_LENGTH);
        assert_eq!(TEST_LENGTH, test_vec.len());

        // Check the contents manually
        for i in 0..TEST_LENGTH {
            assert_eq!(Some(i), test_vec.get(i));
        }

        // Check the contents with an iterator
        {
            let test_iter = test_vec.iter();
            assert_eq!(TEST_LENGTH, test_iter.len());

            for (actual, expected) in test_vec.iter().enumerate() {
                assert_eq!(expected, actual);
            }
        }

        // Manually reverse the vector
        for i in (0..TEST_LENGTH).rev() {
            test_vec = test_vec.assoc(i, TEST_LENGTH - i - 1);
        }

        // Make sure it's reversed
        for i in 0..TEST_LENGTH {
            assert_eq!(Some(TEST_LENGTH - i - 1), test_vec.get(i));
        }

        // Reverse the vector back by mutable ref
        test_vec.visit_mut_elements(&mut |element| {
            *element = TEST_LENGTH - *element - 1;
        });

        // Pop everything off
        for i in (0..TEST_LENGTH).rev() {
            let (new_test_vec, element) = test_vec.pop().unwrap();

            assert_eq!(i, new_test_vec.len());
            assert_eq!(i, element);

            test_vec = new_test_vec;
        }

        assert!(test_vec.is_empty());

        // Extend with three values
        test_vec = test_vec.extend(vec![100, 200, 300].into_iter());
        assert_eq!(3, test_vec.len());

        let collected_values: Vec<usize> = test_vec.iter().collect();
        assert_eq!(vec![100, 200, 300], collected_values);
    }
}
