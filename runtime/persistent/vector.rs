use std::mem::MaybeUninit;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::{alloc, ptr, sync};

/// Reference count used for global constants created by codegen
pub const GLOBAL_CONSTANT_REFCOUNT: u64 = std::u64::MAX;

const TRIE_RADIX: u32 = 5;
pub const NODE_SIZE: usize = 1 << TRIE_RADIX;
const LEVEL_MASK: usize = (1 << TRIE_RADIX) - 1;

#[cfg(test)]
use std::cell::RefCell;

#[cfg(test)]
thread_local! {
    static ALLOCATED_BRANCHES: RefCell<isize> = RefCell::new(0);
}

#[cfg(test)]
thread_local! {
    static ALLOCATED_LEAVES: RefCell<isize> = RefCell::new(0);
}

#[repr(C)]
pub struct Vector<T>
where
    T: Copy,
{
    size: u64,
    root: *const Node<T>,
    tail: *const Node<T>,
}

impl<T> Vector<T>
where
    T: Copy,
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

    /// Pushes a new leaf containing `added_elements` additional elements
    fn push_leaf(&self, leaf: *const Node<T>, added_elements: u64) -> Vector<T> {
        let new_size = self.size + added_elements;
        debug_assert!(new_size & (LEVEL_MASK as u64) == 0);

        let new_root = match unsafe { self.root.as_ref() } {
            None => {
                // We're the first root node
                leaf
            }
            Some(old_root) => {
                let old_depth = Self::trie_depth(self.trie_size());
                let new_depth = Self::trie_depth(self.trie_size() + NODE_SIZE);

                if old_depth == new_depth {
                    old_root.push_leaf(old_depth, new_size as usize - 1, leaf)
                } else {
                    // Need to add a new level
                    let mut root_children: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];
                    root_children[0] = Node::take_ptr_ref(self.root);
                    root_children[1] = Node::new_chain(leaf, old_depth);

                    Node::new_branch(root_children)
                }
            }
        };

        Self {
            size: new_size,
            root: new_root,
            tail: ptr::null(),
        }
    }

    pub fn get(&self, index: usize) -> Option<T> {
        if index >= self.len() {
            return None;
        }

        let leaf_node = self.get_leaf(index);
        unsafe { Some((*leaf_node).elements.leaf[index & LEVEL_MASK].assume_init()) }
    }

    pub fn take(&self, count: usize) -> Vector<T> {
        let new_len = std::cmp::min(count, self.len());
        if new_len == self.len() {
            return self.clone();
        }

        let new_trie_size = Self::trie_size_for_len(new_len);
        let new_root = if new_trie_size > 0 {
            let old_depth = Self::trie_depth(self.trie_size());
            let new_depth = Self::trie_depth(new_trie_size);

            // Drill down until we find our new root
            let new_root = (new_depth..old_depth)
                .fold(self.root, |root, _| unsafe { (*root).elements.branch[0] });

            Node::take_ptr_ref(new_root)
        } else {
            ptr::null()
        };

        let new_tail_size = Self::tail_size_for_len(new_len);
        let new_tail = if new_tail_size > 0 {
            Node::take_ptr_ref(self.get_leaf(new_trie_size))
        } else {
            ptr::null()
        };

        Self {
            size: new_len as u64,
            root: new_root,
            tail: new_tail,
        }
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
            let mut new_elements = [MaybeUninit::uninit(); NODE_SIZE];

            // Copy the previous leaf elements
            new_elements[..self.tail_size()]
                .copy_from_slice(unsafe { &(*self.tail).elements.leaf[..self.tail_size()] });

            // Overwrite the element
            new_elements[index - self.tail_offset()] = MaybeUninit::new(value);

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

    pub fn iter(&self) -> impl ExactSizeIterator<Item = T> + '_ {
        Iter {
            vec: self,
            index: 0,
            current_leaf: self.get_leaf(0),
        }
    }

    pub fn extend(&self, mut values: impl ExactSizeIterator<Item = T>) -> Vector<T> {
        // This is a three step process:
        //
        // 1. Fill the existing tail with values
        // 2. Push whole NODE_SIZE leaves while enough values remain
        // 3. Place the rest of the values in the tail
        //
        // We can run out of values at any phase and return the finished vector

        if values.len() == 0 {
            return self.clone();
        }

        let mut vec_acc = if let Some(tail_ref) = unsafe { self.tail.as_ref() } {
            let old_tail_size = self.tail_size();
            let mut tail_elements = [MaybeUninit::uninit(); NODE_SIZE];

            unsafe {
                tail_elements[..old_tail_size]
                    .copy_from_slice(&tail_ref.elements.leaf[..old_tail_size]);
            }

            let fill_size = std::cmp::min(NODE_SIZE - old_tail_size, values.len());
            let new_tail_size = old_tail_size + fill_size;

            for tail_element in tail_elements.iter_mut().skip(old_tail_size).take(fill_size) {
                *tail_element = MaybeUninit::new(values.next().unwrap());
            }

            let new_leaf = Node::new_leaf(tail_elements);

            if new_tail_size != NODE_SIZE {
                // We only affected the tail
                return Self {
                    size: self.size + (fill_size as u64),
                    root: Node::take_ptr_ref(self.root),
                    tail: new_leaf,
                };
            }

            self.push_leaf(new_leaf, fill_size as u64)
        } else {
            self.clone()
        };

        while values.len() >= NODE_SIZE {
            let mut trie_elements = [MaybeUninit::uninit(); NODE_SIZE];
            for trie_element in &mut trie_elements {
                *trie_element = MaybeUninit::new(values.next().unwrap());
            }

            vec_acc = vec_acc.push_leaf(Node::new_leaf(trie_elements), NODE_SIZE as u64)
        }

        let tail_size = values.len();
        if tail_size > 0 {
            let mut tail_elements = [MaybeUninit::uninit(); NODE_SIZE];
            for (tail_element, value) in tail_elements.iter_mut().zip(values) {
                *tail_element = MaybeUninit::new(value);
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
                tail_ref.visit_mut_elements(0, self.tail_size(), visitor);
            }
        }

        unsafe {
            if let Some(root_ref) = (self.root as *mut Node<T>).as_mut() {
                let trie_size = self.trie_size();
                root_ref.visit_mut_elements(Self::trie_depth(trie_size), trie_size, visitor);
            }
        }
    }

    /// Size of the trie portion of the `Vector`
    ///
    /// This is always a multiple of `NODE_SIZE`
    fn trie_size(&self) -> usize {
        Self::trie_size_for_len(self.len())
    }

    fn trie_size_for_len(len: usize) -> usize {
        len - Self::tail_size_for_len(len)
    }

    /// Size of the tail portion of the `Vector`
    ///
    /// This is always less than `NODE_SIZE`
    fn tail_size(&self) -> usize {
        Self::tail_size_for_len(self.len())
    }

    fn tail_size_for_len(len: usize) -> usize {
        len % NODE_SIZE
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
    T: Copy,
{
    fn drop(&mut self) {
        unsafe {
            Node::release_ptr_ref(self.root, Self::trie_depth(self.trie_size()));
            Node::release_ptr_ref(self.tail, 0);
        }
    }
}

union NodeElements<T>
where
    T: Copy,
{
    leaf: [MaybeUninit<T>; NODE_SIZE],
    branch: [*const Node<T>; NODE_SIZE],
}

#[repr(C)]
struct Node<T>
where
    T: Copy,
{
    ref_count: AtomicU64,
    elements: NodeElements<T>,
}

impl<T> Node<T>
where
    T: Copy,
{
    fn new_leaf(elements: [MaybeUninit<T>; NODE_SIZE]) -> *const Node<T> {
        #[cfg(test)]
        ALLOCATED_LEAVES.with(|counter| *counter.borrow_mut() += 1);

        let layout = alloc::Layout::new::<Self>();

        unsafe {
            let node = alloc::alloc(layout) as *mut Node<T>;

            (*node).ref_count = AtomicU64::new(1);
            (*node).elements.leaf = elements;

            node
        }
    }

    fn new_branch(elements: [*const Node<T>; NODE_SIZE]) -> *const Node<T> {
        #[cfg(test)]
        ALLOCATED_BRANCHES.with(|counter| *counter.borrow_mut() += 1);

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
            new_elements[index & LEVEL_MASK] = MaybeUninit::new(value);

            return Self::new_leaf(new_elements);
        }

        let level_radix = TRIE_RADIX * remaining_depth;
        let branch_index = (index >> level_radix) & LEVEL_MASK;

        // Replace the branch value
        let new_subtree = unsafe {
            (&*self.elements.branch[branch_index]).assoc_value(remaining_depth - 1, index, value)
        };

        let mut new_elements: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];

        for (i, new_element) in new_elements.iter_mut().enumerate() {
            unsafe {
                *new_element = if i == branch_index {
                    new_subtree
                } else {
                    Node::take_ptr_ref(self.elements.branch[i])
                };
            }
        }

        Self::new_branch(new_elements)
    }

    fn push_leaf(
        &self,
        remaining_depth: u32,
        last_index: usize,
        leaf: *const Node<T>,
    ) -> *const Node<T> {
        if remaining_depth == 0 {
            return leaf;
        }

        let level_radix = TRIE_RADIX * remaining_depth;
        let branch_index = (last_index >> level_radix) & LEVEL_MASK;

        // Replace the branch value
        let new_subtree = unsafe {
            match self.elements.branch[branch_index].as_ref() {
                Some(branch) => branch.push_leaf(remaining_depth - 1, last_index, leaf),
                None => Self::new_chain(leaf, remaining_depth - 1),
            }
        };

        let mut new_elements: [*const Node<T>; NODE_SIZE] = [ptr::null(); NODE_SIZE];

        for (new_element, old_element) in new_elements
            .iter_mut()
            .zip(unsafe { self.elements.branch }.iter())
            .take(branch_index)
        {
            *new_element = Node::take_ptr_ref(*old_element)
        }

        new_elements[branch_index] = new_subtree;
        Self::new_branch(new_elements)
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

                #[cfg(test)]
                ALLOCATED_BRANCHES.with(|counter| *counter.borrow_mut() -= 1);
            } else {
                #[cfg(test)]
                ALLOCATED_LEAVES.with(|counter| *counter.borrow_mut() -= 1);
            }

            alloc::dealloc(
                self_ref as *const Self as *mut u8,
                alloc::Layout::new::<Self>(),
            );
        }
    }

    /// Visits up to `remaining_elements` mutable elements, returning the new remaining count
    fn visit_mut_elements<F>(
        &mut self,
        remaining_depth: u32,
        mut remaining_elements: usize,
        visitor: &mut F,
    ) -> usize
    where
        F: FnMut(&mut T),
    {
        if self.is_global_constant() {
            // We're a global constant; skip us
            return remaining_elements.saturating_sub(NODE_SIZE << (remaining_depth * TRIE_RADIX));
        }

        if remaining_depth == 0 {
            let leaf_size = std::cmp::min(remaining_elements, NODE_SIZE);

            unsafe {
                for element in self.elements.leaf.iter_mut().take(leaf_size) {
                    visitor(&mut (*element.as_mut_ptr()));
                }
            }

            return remaining_elements - leaf_size;
        }

        for branch in unsafe { self.elements.branch.iter() } {
            unsafe {
                remaining_elements = (&mut *(*branch as *mut Node<T>)).visit_mut_elements(
                    remaining_depth - 1,
                    remaining_elements,
                    visitor,
                );
            }

            if remaining_elements == 0 {
                return 0;
            }
        }

        remaining_elements
    }
}

struct Iter<'a, T>
where
    T: Copy,
{
    vec: &'a Vector<T>,
    index: usize,
    current_leaf: *const Node<T>,
}

impl<'a, T> Iterator for Iter<'a, T>
where
    T: Copy,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.index >= self.vec.len() {
            return None;
        }

        let item =
            unsafe { (*self.current_leaf).elements.leaf[self.index & LEVEL_MASK].assume_init() };

        self.index += 1;
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

impl<'a, T> ExactSizeIterator for Iter<'a, T> where T: Copy {}

impl<T> Clone for Vector<T>
where
    T: Copy,
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

    fn assert_nodes_deallocated<T>(block: T)
    where
        T: FnOnce(),
    {
        assert_eq!(
            0,
            ALLOCATED_BRANCHES.with(|counter| *counter.borrow()),
            "branches allocated before beginning of test"
        );

        assert_eq!(
            0,
            ALLOCATED_LEAVES.with(|counter| *counter.borrow()),
            "leaves allocated before beginning of test"
        );

        block();

        assert_eq!(
            0,
            ALLOCATED_BRANCHES.with(|counter| *counter.borrow()),
            "branches still allocated after end of test"
        );

        assert_eq!(
            0,
            ALLOCATED_LEAVES.with(|counter| *counter.borrow()),
            "leaves still allocated after end of test"
        );
    }

    #[test]
    fn tail_only_vector() {
        assert_nodes_deallocated(|| {
            let empty_vec = Vector::<i32>::new(iter::empty());

            assert_eq!(0, empty_vec.len());
            assert!(empty_vec.is_empty());

            let one_vec = empty_vec.extend(iter::once(0));

            // Make sure `empty_vec` is still intact
            assert_eq!(0, empty_vec.len());
            assert!(empty_vec.is_empty());
            assert_eq!(None, empty_vec.get(0));

            assert_eq!(1, one_vec.len());
            assert!(!one_vec.is_empty());
            assert_eq!(Some(0), one_vec.get(0));

            // Try modifying the original one item vec
            let mutated_vec = one_vec.assoc(0, 31337);

            assert_eq!(1, mutated_vec.len());
            assert!(!mutated_vec.is_empty());
            assert_eq!(Some(31337), mutated_vec.get(0));
            assert_eq!(Some(0), one_vec.get(0));
        });
    }

    #[test]
    fn extended_one_level_vector() {
        assert_nodes_deallocated(|| {
            const TEST_LEN: usize = 48;

            let mut test_vec = Vector::<usize>::new(iter::empty());

            for i in 0..TEST_LEN {
                assert_eq!(i, test_vec.len());
                test_vec = test_vec.extend(iter::once(i));
            }

            // Check the contents manually
            for i in 0..TEST_LEN {
                assert_eq!(Some(i), test_vec.get(i));
            }

            // Check the contents with an iterator
            {
                let test_iter = test_vec.iter();
                assert_eq!(TEST_LEN, test_iter.len());

                for (actual, expected) in test_vec.iter().enumerate() {
                    assert_eq!(expected, actual);
                }
            }
        })
    }

    #[test]
    fn extended_two_level_vector() {
        assert_nodes_deallocated(|| {
            const TEST_LEN: usize = 128;

            let mut test_vec = Vector::<usize>::new(iter::empty());

            for i in 0..TEST_LEN {
                assert_eq!(i, test_vec.len());
                test_vec = test_vec.extend(iter::once(i));
            }

            // Check the contents manually
            for i in 0..TEST_LEN {
                assert_eq!(Some(i), test_vec.get(i));
            }

            // Check the contents with an iterator
            {
                let test_iter = test_vec.iter();
                assert_eq!(TEST_LEN, test_iter.len());

                for (actual, expected) in test_vec.iter().enumerate() {
                    assert_eq!(expected, actual);
                }
            }

            // Check the contents using take
            for i in (0..TEST_LEN).step_by(3) {
                let head_vec = test_vec.take(i);
                assert_eq!(i, head_vec.len());

                if i > 0 {
                    assert_eq!(Some(0), head_vec.get(0));
                    assert_eq!(Some(i - 1), head_vec.get(i - 1));
                }
            }
        })
    }

    #[test]
    fn initialised_three_level_vector() {
        assert_nodes_deallocated(|| {
            const TEST_LEN: usize = 2087;

            let mut test_vec = Vector::<usize>::new(0..TEST_LEN);
            assert_eq!(TEST_LEN, test_vec.len());

            // Check the contents manually
            for i in 0..TEST_LEN {
                assert_eq!(Some(i), test_vec.get(i));
            }

            // Check the contents with an iterator
            {
                let test_iter = test_vec.iter();
                assert_eq!(TEST_LEN, test_iter.len());

                for (actual, expected) in test_vec.iter().enumerate() {
                    assert_eq!(expected, actual);
                }
            }

            // Manually reverse the vector
            for i in (0..TEST_LEN).rev() {
                test_vec = test_vec.assoc(i, TEST_LEN - i - 1);
            }

            // Make sure it's reversed
            for i in 0..TEST_LEN {
                assert_eq!(Some(TEST_LEN - i - 1), test_vec.get(i));
            }

            // Reverse the vector back by mutable ref
            test_vec.visit_mut_elements(&mut |element| {
                *element = TEST_LEN - *element - 1;
            });

            // Check the contents using take
            for i in (0..TEST_LEN).step_by(7) {
                let head_vec = test_vec.take(i);
                assert_eq!(i, head_vec.len());

                for (actual, expected) in head_vec.iter().enumerate() {
                    assert_eq!(expected, actual);
                }
            }
        })
    }

    #[test]
    fn vector_extend() {
        assert_nodes_deallocated(|| {
            let start_vec = Vector::<usize>::new(1..4);
            let extended_vec = start_vec.extend(4..7);

            let all_values: Vec<usize> = extended_vec.iter().collect();
            assert_eq!(vec![1, 2, 3, 4, 5, 6], all_values);

            let zero_extended_vec = extended_vec.extend(iter::empty());
            assert_eq!(6, zero_extended_vec.len());
        })
    }
}
