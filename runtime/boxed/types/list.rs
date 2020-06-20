use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FusedIterator;
use std::marker::PhantomData;

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::boxed::*;

/// Non-empty list
#[repr(C, align(16))]
pub struct Pair<T: Boxed = Any> {
    header: Header,
    list_len: i64,
    pub(crate) head: Gc<T>,
    pub(crate) rest: Gc<List<T>>,
}

impl<T: Boxed> Boxed for Pair<T> {}
impl<T: Boxed> EncodeBoxedABIType for Pair<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Pair(&T::BOXED_ABI_TYPE);
}

impl<T: Boxed> Pair<T> {
    /// Constructs a pair with the given `head` and `rest`
    pub fn new(heap: &mut impl AsHeap, head: Gc<T>, rest: Gc<List<T>>) -> Gc<Pair<T>> {
        heap.as_heap_mut().place_box(Pair {
            header: Pair::TYPE_TAG.to_heap_header(Self::size()),
            head,
            rest,
            list_len: (rest.len() + 1) as i64,
        })
    }

    /// Returns the box size for pairs
    pub fn size() -> BoxSize {
        BoxSize::Size32
    }

    /// Returns the length of the list this pair is the head of
    ///
    /// Note that this must be at least 1.
    pub fn len(&self) -> usize {
        self.list_len as usize
    }

    /// Returns false
    pub fn is_empty(&self) -> bool {
        // This is to make Clippy happy since we have `len`
        false
    }

    /// Returns the head value
    pub fn head(&self) -> Gc<T> {
        self.head
    }

    /// Returns the tail list
    pub fn rest(&self) -> Gc<List<T>> {
        self.rest
    }

    /// Casts this pair to a non-empty list
    pub fn as_list_ref(&self) -> Gc<List<T>> {
        unsafe { Gc::new(&*(self as *const _ as *const List<T>)) }
    }
}

impl<T: Boxed> PartialEqInHeap for Pair<T> {
    fn eq_in_heap(&self, heap: &Heap, rhs: &Pair<T>) -> bool {
        self.head.eq_in_heap(heap, &rhs.head) && self.rest.eq_in_heap(heap, &rhs.rest)
    }
}

impl<T: Boxed> HashInHeap for Pair<T> {
    fn hash_in_heap<H: Hasher>(&self, task: &Heap, state: &mut H) {
        TypeTag::Pair.hash(state);
        self.head().hash_in_heap(task, state);
        self.rest().hash_in_heap(task, state);
    }
}

impl<T: Boxed> fmt::Debug for Pair<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.as_list_ref().fmt(formatter)
    }
}

/// List of boxed values
///
/// This allows O(n) access to its elements. It has the benefit of allowing constant time prepends
/// while sharing the tail of the existing list.
#[repr(C, align(16))]
pub struct List<T: Boxed = Any> {
    header: Header,
    list_len: i64,
    phantom: PhantomData<T>,
}

impl<T: Boxed> Boxed for List<T> {}

impl DistinctTagged for List<Any> {
    fn has_tag(type_tag: TypeTag) -> bool {
        [TypeTag::Pair, TypeTag::Nil].contains(&type_tag)
    }
}

impl<T: Boxed> EncodeBoxedABIType for List<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::List(&T::BOXED_ABI_TYPE);
}

/// Possible subtypes of [`List`]
pub enum ListSubtype<'a, T: Boxed> {
    /// Non-empty list
    Pair(&'a Pair<T>),
    /// Empty list
    Nil,
}

impl<T: Boxed> List<T> {
    /// Constructs a new fixed sized list containing the passed `elems`
    #[allow(clippy::new_ret_no_self)]
    pub fn new(heap: &mut impl AsHeap, elems: impl ExactSizeIterator<Item = Gc<T>>) -> Gc<List<T>> {
        Self::new_with_tail(heap, elems, Self::empty())
    }

    /// Constructs a list with a head of `elems` and the specified tail list
    pub fn new_with_tail(
        heap: &mut impl AsHeap,
        elems: impl ExactSizeIterator<Item = Gc<T>>,
        tail: Gc<List<T>>,
    ) -> Gc<List<T>> {
        let elems_len = elems.len();
        let tail_len = tail.len();

        if elems_len == 0 {
            return tail;
        }

        // Allocate the entire list at once
        let heap_alloc = heap
            .as_heap_mut()
            .alloc_cells(Pair::<T>::size().cell_count() * elems_len);

        unsafe {
            let pair_alloc = heap_alloc as *mut Pair<T>;

            for (i, head) in elems.enumerate() {
                let elems_remaining = elems_len - i;

                let rest = if elems_remaining == 1 {
                    tail
                } else {
                    (&*pair_alloc.add(i + 1)).as_list_ref()
                };

                *pair_alloc.add(i) = Pair {
                    header: Pair::TYPE_TAG.to_heap_header(Pair::<T>::size()),
                    head,
                    rest,
                    list_len: (elems_remaining + tail_len) as i64,
                };
            }

            Gc::new(pair_alloc as *const List<T>)
        }
    }

    /// Returns an empty list
    pub fn empty() -> Gc<List<T>> {
        unsafe { Gc::new(&NIL_INSTANCE as *const Nil as *const List<T>) }
    }

    /// Creates a list by constructing an iterator of values
    pub fn from_values<V, F>(
        heap: &mut impl AsHeap,
        values: impl Iterator<Item = V>,
        cons: F,
    ) -> Gc<List<T>>
    where
        F: Fn(&mut Heap, V) -> Gc<T>,
    {
        let heap = heap.as_heap_mut();

        let elems: Vec<Gc<T>> = values.map(|v| cons(heap, v)).collect();
        Self::new(heap, elems.into_iter())
    }

    /// Returns a subtype of this list based on its type tag
    pub fn as_subtype(&self) -> ListSubtype<'_, T> {
        match self.header.type_tag {
            TypeTag::Pair => {
                ListSubtype::Pair(unsafe { &*(self as *const List<T> as *const Pair<T>) })
            }
            TypeTag::Nil => ListSubtype::Nil,
            other => {
                unreachable!("Unexpected type tag: {:?}", other);
            }
        }
    }

    /// Returns the length of the list
    pub fn len(&self) -> usize {
        self.list_len as usize
    }

    /// Returns true if the list is empty
    pub fn is_empty(&self) -> bool {
        self.header.type_tag == TypeTag::Nil
    }

    /// Returns an iterator to the list's values
    pub fn iter(&self) -> ListIterator<T> {
        ListIterator {
            head: unsafe { Gc::new(self as *const Self) },
        }
    }
}

impl<T: Boxed> PartialEqInHeap for List<T> {
    fn eq_in_heap(&self, heap: &Heap, other: &List<T>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.iter()
            .zip(other.iter())
            .all(|(self_value, other_value)| self_value.eq_in_heap(heap, &other_value))
    }
}

impl<T: Boxed> HashInHeap for List<T> {
    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
        match self.as_subtype() {
            ListSubtype::Pair(pair) => pair.hash_in_heap(heap, state),
            ListSubtype::Nil => NIL_INSTANCE.hash_in_heap(heap, state),
        }
    }
}

impl<T: Boxed> fmt::Debug for List<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        formatter.write_str("List(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

pub struct ListIterator<T: Boxed> {
    head: Gc<List<T>>,
}

impl<T: Boxed> Iterator for ListIterator<T> {
    type Item = Gc<T>;

    fn next(&mut self) -> Option<Gc<T>> {
        // If we use `head` directly the borrow checker gets suspicious
        let head = unsafe { &*(self.head.as_ptr()) };

        match head.as_subtype() {
            ListSubtype::Pair(pair) => {
                self.head = pair.rest;
                Some(pair.head)
            }
            ListSubtype::Nil => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.head.len(), Some(self.head.len()))
    }
}

impl<T: Boxed> ExactSizeIterator for ListIterator<T> {}
impl<T: Boxed> FusedIterator for ListIterator<T> {}

/// Empty list
#[repr(C, align(16))]
#[derive(Debug)]
pub struct Nil {
    header: Header,
    list_len: usize,
}

/// Static constant instance of [`Nil`]
#[export_name = "ARRET_NIL"]
pub static NIL_INSTANCE: Nil = Nil {
    header: Header {
        type_tag: TypeTag::Nil,
        alloc_type: AllocType::Const,
    },
    list_len: 0,
};

impl Boxed for Nil {}
impl UniqueTagged for Nil {}

impl PartialEq for Nil {
    fn eq(&self, _: &Nil) -> bool {
        true
    }
}

impl Hash for Nil {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        state.write_usize(&NIL_INSTANCE as *const _ as usize);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use crate::boxed::Int;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Nil>());
        assert_eq!(16, mem::size_of::<List<Any>>());
        assert_eq!(32, mem::size_of::<Pair<Any>>());
    }

    #[test]
    fn equality() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let forward_list1 = List::from_values(&mut heap, [1, 2, 3].iter().cloned(), Int::new);
        let forward_list2 = List::from_values(&mut heap, [1, 2, 3].iter().cloned(), Int::new);
        let reverse_list = List::from_values(&mut heap, [3, 2, 1].iter().cloned(), Int::new);

        assert_eq!(false, forward_list1.eq_in_heap(&heap, &reverse_list));
        assert_eq!(true, forward_list1.eq_in_heap(&heap, &forward_list2));
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();
        let forward_list = List::from_values(&mut heap, [1, 2, 3].iter().cloned(), Int::new);

        assert_eq!(
            "List([Int(1), Int(2), Int(3)])",
            format!("{:?}", forward_list)
        );
    }

    #[test]
    fn construct_and_iter() {
        let mut heap = Heap::empty();

        let boxed_list = List::from_values(&mut heap, [1, 2, 3].iter().cloned(), Int::new);

        let mut boxed_list_iter = boxed_list.iter();
        assert_eq!(3, boxed_list_iter.len());

        for expected_num in &[1, 2, 3] {
            if let Some(boxed_int) = boxed_list_iter.next() {
                assert_eq!(*expected_num, boxed_int.value());
            } else {
                panic!("Iterator unexpectedly ended");
            }
        }

        assert_eq!(0, boxed_list_iter.len());
        assert_eq!(false, boxed_list_iter.next().is_some());
    }
}
