use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::{fmt, marker, mem};

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::boxed::*;

const MAX_16BYTE_INLINE_LENGTH: usize = (16 - 8) / mem::size_of::<Gc<Any>>();
const MAX_32BYTE_INLINE_LENGTH: usize = (32 - 8) / mem::size_of::<Gc<Any>>();

/// Describes the storage of a set's data
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SetStorage {
    /// Set data is stored inline in a box of the given size
    Inline(BoxSize),
    /// Set data is stored out-of-line in a 32 byte box
    External,
}

impl SetStorage {
    /// Returns the box size for a set storage
    pub fn box_size(self) -> BoxSize {
        match self {
            SetStorage::Inline(box_size) => box_size,
            SetStorage::External => BoxSize::Size32,
        }
    }
}

/// Immutable set of boxed values
///
/// This is semantically similar to a map of values to the unit type
#[repr(C, align(16))]
pub struct Set<T: Boxed = Any> {
    header: Header,
    inline_length: u32,
    padding: [u8; 24],
    phantom: marker::PhantomData<T>,
}

impl<T: Boxed> Boxed for Set<T> {}

impl<T: Boxed> Set<T> {
    /// Maximum element length of an inline set
    pub const MAX_INLINE_LENGTH: usize = MAX_32BYTE_INLINE_LENGTH;

    /// Constructs a new set with the passed boxed values
    pub fn new(heap: &mut impl AsHeap, values: impl ExactSizeIterator<Item = Gc<T>>) -> Gc<Set<T>> {
        let heap = heap.as_heap_mut();

        // Calculate the hash of our values
        let mut hashed_values: Vec<(u64, Gc<T>)> = values
            .map(|v| {
                let mut state = DefaultHasher::new();
                v.hash_in_heap(heap, &mut state);
                (state.finish(), v)
            })
            .collect();

        // Make the values sorted and unique
        hashed_values.sort_by_key(|(hash, _)| *hash);
        hashed_values
            .dedup_by(|(hash1, v1), (hash2, v2)| hash1 == hash2 && v1.eq_in_heap(heap, v2));

        let storage = Self::storage_for_element_len(hashed_values.len());
        let header = Set::TYPE_TAG.to_heap_header(storage.box_size());

        let boxed = unsafe {
            match storage {
                SetStorage::External => mem::transmute(ExternalSet::new(header, hashed_values)),
                SetStorage::Inline(_) => mem::transmute(InlineSet::new(header, hashed_values)),
            }
        };

        heap.place_box(boxed)
    }

    /// Returns the storage for given element length
    fn storage_for_element_len(len: usize) -> SetStorage {
        const MIN_32BYTE_INLINE_LENGTH: usize = MAX_16BYTE_INLINE_LENGTH + 1;

        match len {
            0..=MAX_16BYTE_INLINE_LENGTH => SetStorage::Inline(BoxSize::Size16),
            MIN_32BYTE_INLINE_LENGTH..=MAX_32BYTE_INLINE_LENGTH => {
                SetStorage::Inline(BoxSize::Size32)
            }
            _ => {
                // Too big to fit inline; this needs to be external
                SetStorage::External
            }
        }
    }

    /// Constructs a set by constructing an iterator of values
    pub fn from_values<V, F>(
        heap: &mut impl AsHeap,
        values: impl Iterator<Item = V>,
        cons: F,
    ) -> Gc<Set<T>>
    where
        F: Fn(&mut Heap, V) -> Gc<T>,
    {
        let heap = heap.as_heap_mut();

        let elems: Vec<Gc<T>> = values.map(|v| cons(heap, v)).collect();
        Self::new(heap, elems.into_iter())
    }

    fn is_inline(&self) -> bool {
        self.inline_length <= (Self::MAX_INLINE_LENGTH as u32)
    }

    fn as_repr(&self) -> Repr<'_, T> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Set<T> as *const InlineSet<T>) })
        } else {
            Repr::External(unsafe { &*(self as *const Set<T> as *const ExternalSet<T>) })
        }
    }

    /// Returns the length of the set
    pub fn len(&self) -> usize {
        match self.as_repr() {
            Repr::Inline(inline) => inline.len(),
            Repr::External(external) => external.len(),
        }
    }

    /// Returns true if the set is empty
    pub fn is_empty(&self) -> bool {
        self.inline_length == 0
    }

    /// Returns true if the passed value is included in the set
    pub fn contains(&self, heap: &Heap, value: &Gc<T>) -> bool {
        match self.as_repr() {
            Repr::Inline(inline) => inline.contains(heap, value),
            Repr::External(external) => external.contains(heap, value),
        }
    }

    /// Returns an iterator over the set
    pub fn iter<'a>(&'a self) -> Box<dyn ExactSizeIterator<Item = &'a Gc<T>> + 'a> {
        // TODO: It would be nice not to box here
        match self.as_repr() {
            Repr::Inline(inline) => Box::new(inline.iter()),
            Repr::External(external) => Box::new(external.iter()),
        }
    }

    /// Returns if this set is a subset of the passed set
    pub fn is_subset(&self, heap: &Heap, other: &Set<T>) -> bool {
        match (self.as_repr(), other.as_repr()) {
            (Repr::External(external_self), Repr::External(external_other)) => {
                // Use optimised external/external logic
                external_self.is_subset(heap, external_other)
            }
            _ => {
                if self.len() > other.len() {
                    return false;
                }

                for self_value in self.iter() {
                    if !other.contains(heap, self_value) {
                        return false;
                    }
                }

                true
            }
        }
    }
}

impl<T: Boxed> PartialEqInHeap for Set<T> {
    fn eq_in_heap(&self, heap: &Heap, other: &Set<T>) -> bool {
        match (self.as_repr(), other.as_repr()) {
            (Repr::Inline(self_inline), Repr::Inline(other_inline)) => {
                self_inline.eq_in_heap(heap, other_inline)
            }
            (Repr::External(self_external), Repr::External(other_external)) => {
                self_external.eq_in_heap(heap, other_external)
            }
            _ => false,
        }
    }
}

impl<T: Boxed> HashInHeap for Set<T> {
    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
        match self.as_repr() {
            Repr::Inline(inline) => inline.hash_in_heap(heap, state),
            Repr::External(external) => external.hash_in_heap(heap, state),
        }
    }
}

impl<T: Boxed> fmt::Debug for Set<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        formatter.write_str("Set(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<T: Boxed> EncodeBoxedABIType for Set<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Set(&T::BOXED_ABI_TYPE);
}

#[repr(C, align(16))]
pub struct InlineSet<T: Boxed> {
    header: Header,
    inline_length: u32,
    values: [Gc<T>; MAX_32BYTE_INLINE_LENGTH],
}

impl<T: Boxed> InlineSet<T> {
    fn new(header: Header, hashed_values: Vec<(u64, Gc<T>)>) -> InlineSet<T> {
        unsafe {
            let inline_length = hashed_values.len();

            let mut inline_values =
                mem::MaybeUninit::<[Gc<T>; MAX_32BYTE_INLINE_LENGTH]>::uninit().assume_init();

            for (inline_value, (_, value)) in inline_values.iter_mut().zip(hashed_values) {
                ptr::write(inline_value, value);
            }

            InlineSet {
                header,
                inline_length: inline_length as u32,
                values: inline_values,
            }
        }
    }

    fn len(&self) -> usize {
        self.inline_length as usize
    }

    fn iter(&self) -> impl ExactSizeIterator<Item = &Gc<T>> {
        self.values[0..self.inline_length as usize].iter()
    }

    fn contains(&self, heap: &Heap, value: &Gc<T>) -> bool {
        self.iter().any(|v| v.eq_in_heap(heap, &value))
    }

    fn eq_in_heap(&self, heap: &Heap, other: &InlineSet<T>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.iter()
            .zip(other.iter())
            .all(|(self_value, other_value)| self_value.eq_in_heap(heap, other_value))
    }

    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
        TypeTag::Set.hash(state);
        state.write_usize(self.len());
        for value in self.iter() {
            value.hash_in_heap(heap, state);
        }
    }
}

#[repr(C, align(16))]
pub struct ExternalSet<T: Boxed> {
    header: Header,
    inline_length: u32,
    sorted_hashed_values: Vec<(u64, Gc<T>)>,
}

impl<T: Boxed> ExternalSet<T> {
    fn new(header: Header, sorted_hashed_values: Vec<(u64, Gc<T>)>) -> ExternalSet<T> {
        ExternalSet {
            header,
            inline_length: (Set::<T>::MAX_INLINE_LENGTH + 1) as u32,
            sorted_hashed_values,
        }
    }

    fn len(&self) -> usize {
        self.sorted_hashed_values.len()
    }

    fn iter(&self) -> impl ExactSizeIterator<Item = &Gc<T>> {
        self.sorted_hashed_values.iter().map(|(_, v)| v)
    }

    fn contains(&self, heap: &Heap, needle_value: &Gc<T>) -> bool {
        // Hash our value
        let mut state = DefaultHasher::new();
        needle_value.hash_in_heap(heap, &mut state);
        let needle_hash = state.finish();

        // Do a binary search for the index
        // This will return an arbitrary matching index if there are multiple matches
        let arbitrary_index = if let Ok(i) = self
            .sorted_hashed_values
            .binary_search_by_key(&needle_hash, |(haystack_hash, _)| *haystack_hash)
        {
            i
        } else {
            return false;
        };

        // Search forwards through hash collisions, including the arbitrary index
        let mut forwards_index = arbitrary_index;
        loop {
            let (hackstack_hash, haystack_value) = self.sorted_hashed_values[forwards_index];

            if hackstack_hash != needle_hash {
                break;
            }
            if haystack_value.eq_in_heap(heap, &needle_value) {
                return true;
            }

            forwards_index += 1;
            if forwards_index >= self.sorted_hashed_values.len() {
                break;
            }
        }

        // Search backwards through hash collisions
        let mut backwards_index = arbitrary_index;
        while backwards_index > 0 {
            backwards_index -= 1;
            let (hackstack_hash, haystack_value) = self.sorted_hashed_values[backwards_index];

            if hackstack_hash != needle_hash {
                break;
            }
            if haystack_value.eq_in_heap(heap, &needle_value) {
                return true;
            }
        }

        false
    }

    /// Returns if this set is a subset of the passed set
    fn is_subset(&self, heap: &Heap, other: &ExternalSet<T>) -> bool {
        let mut other_index = 0;

        for (self_hash, self_value) in self.sorted_hashed_values.iter() {
            loop {
                let (other_hash, other_value) =
                    if let Some(entry) = other.sorted_hashed_values.get(other_index) {
                        entry
                    } else {
                        // Ran past the end of the other set
                        return false;
                    };

                if other_hash == self_hash && other_value.eq_in_heap(heap, self_value) {
                    // Found corresponding element
                    break;
                } else if other_hash > self_hash {
                    // We've gone past where the corresponding element should be
                    return false;
                }

                other_index += 1;
            }
        }

        true
    }

    fn eq_in_heap(&self, heap: &Heap, other: &ExternalSet<T>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.sorted_hashed_values
            .iter()
            .zip(other.sorted_hashed_values.iter())
            .all(|((self_hash, self_value), (other_hash, other_value))| {
                self_hash == other_hash && self_value.eq_in_heap(heap, other_value)
            })
    }

    fn hash_in_heap<H: Hasher>(&self, _: &Heap, state: &mut H) {
        TypeTag::Set.hash(state);
        state.write_usize(self.len());

        // Instead of recursing into values, use their pre-calculated hash
        for (hash, _) in self.sorted_hashed_values.iter() {
            state.write_u64(*hash);
        }
    }
}

enum Repr<'a, T: Boxed> {
    Inline(&'a InlineSet<T>),
    External(&'a ExternalSet<T>),
}

impl<T: Boxed> Drop for Set<T> {
    fn drop(&mut self) {
        match self.as_repr() {
            Repr::Inline(_) => {
                // Do nothing here; we might've been allocated as a 16 byte box so we can't read
                // the whole thing.
            }
            Repr::External(external) => unsafe {
                // Call `ExternalSet`'s drop implementation
                ptr::read(external);
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Set<Any>>());
        assert_eq!(32, mem::size_of::<InlineSet<Any>>());
        assert_eq!(32, mem::size_of::<ExternalSet<Any>>());
    }

    #[test]
    fn inline_equality() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);

        let forward_set1 = Set::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());
        let forward_set2 = Set::new(&mut heap, vec![boxed1, boxed2, boxed2, boxed3].into_iter());
        let reverse_set = Set::new(&mut heap, vec![boxed3, boxed2, boxed1].into_iter());

        let partial_set = Set::new(&mut heap, vec![boxed1, boxed3].into_iter());

        assert_eq!(true, forward_set1.eq_in_heap(&heap, &reverse_set));
        assert_eq!(true, forward_set1.eq_in_heap(&heap, &forward_set2));
        assert_eq!(false, forward_set1.eq_in_heap(&heap, &partial_set));
    }

    #[test]
    fn inline_contains() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);
        let boxed4 = Int::new(&mut heap, 4);
        let boxed5 = Int::new(&mut heap, 5);

        let empty_set = Set::<Int>::new(&mut heap, vec![].into_iter());
        let odd_set = Set::new(&mut heap, vec![boxed1, boxed3, boxed5].into_iter());
        let even_set = Set::new(&mut heap, vec![boxed2, boxed4].into_iter());

        assert_eq!(false, empty_set.contains(&heap, &boxed1));
        assert_eq!(true, odd_set.contains(&heap, &boxed1));
        assert_eq!(false, even_set.contains(&heap, &boxed1));

        assert_eq!(false, empty_set.contains(&heap, &boxed2));
        assert_eq!(false, odd_set.contains(&heap, &boxed2));
        assert_eq!(true, even_set.contains(&heap, &boxed2));

        assert_eq!(false, empty_set.contains(&heap, &boxed3));
        assert_eq!(true, odd_set.contains(&heap, &boxed3));
        assert_eq!(false, even_set.contains(&heap, &boxed3));
    }

    #[test]
    fn external_equality() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);
        let boxed4 = Int::new(&mut heap, 4);
        let boxed5 = Int::new(&mut heap, 5);

        let forward_set = Set::new(
            &mut heap,
            vec![boxed1, boxed2, boxed3, boxed4, boxed5].into_iter(),
        );

        let reverse_set = Set::new(
            &mut heap,
            vec![boxed5, boxed4, boxed3, boxed2, boxed1].into_iter(),
        );

        let inline_set = Set::new(&mut heap, vec![boxed1, boxed2, boxed3, boxed4].into_iter());
        let empty_set = Set::<Int>::new(&mut heap, vec![].into_iter());

        assert_eq!(true, forward_set.eq_in_heap(&heap, &reverse_set));
        assert_eq!(false, forward_set.eq_in_heap(&heap, &inline_set));
        assert_eq!(false, forward_set.eq_in_heap(&heap, &empty_set));
    }
    #[test]
    fn external_contains() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);
        let boxed4 = Int::new(&mut heap, 4);
        let boxed5 = Int::new(&mut heap, 5);
        let boxed6 = Int::new(&mut heap, 6);
        let boxed7 = Int::new(&mut heap, 7);
        let boxed8 = Int::new(&mut heap, 8);

        let empty_set = Set::<Int>::new(&mut heap, vec![].into_iter());
        let odd_set = Set::new(&mut heap, vec![boxed1, boxed3, boxed5, boxed7].into_iter());
        let even_set = Set::new(&mut heap, vec![boxed2, boxed4, boxed6, boxed8].into_iter());

        assert_eq!(false, empty_set.contains(&heap, &boxed1));
        assert_eq!(true, odd_set.contains(&heap, &boxed1));
        assert_eq!(false, even_set.contains(&heap, &boxed1));

        assert_eq!(false, empty_set.contains(&heap, &boxed2));
        assert_eq!(false, odd_set.contains(&heap, &boxed2));
        assert_eq!(true, even_set.contains(&heap, &boxed2));

        assert_eq!(false, empty_set.contains(&heap, &boxed3));
        assert_eq!(true, odd_set.contains(&heap, &boxed3));
        assert_eq!(false, even_set.contains(&heap, &boxed3));
    }

    #[test]
    fn subset() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);
        let boxed4 = Int::new(&mut heap, 4);
        let boxed5 = Int::new(&mut heap, 5);
        let boxed6 = Int::new(&mut heap, 6);
        let boxed7 = Int::new(&mut heap, 7);
        let boxed8 = Int::new(&mut heap, 8);

        let empty_set = Set::<Int>::new(&mut heap, vec![].into_iter());
        let odd_set = Set::new(&mut heap, vec![boxed1, boxed3, boxed5, boxed7].into_iter());
        let even_set = Set::new(&mut heap, vec![boxed2, boxed4, boxed6, boxed8].into_iter());
        let full_set = Set::new(
            &mut heap,
            vec![
                boxed1, boxed2, boxed3, boxed4, boxed5, boxed6, boxed7, boxed8,
            ]
            .into_iter(),
        );

        assert_eq!(true, empty_set.is_subset(&heap, &empty_set));
        assert_eq!(true, empty_set.is_subset(&heap, &odd_set));
        assert_eq!(true, empty_set.is_subset(&heap, &even_set));
        assert_eq!(true, empty_set.is_subset(&heap, &full_set));

        assert_eq!(false, odd_set.is_subset(&heap, &empty_set));
        assert_eq!(true, odd_set.is_subset(&heap, &odd_set));
        assert_eq!(false, odd_set.is_subset(&heap, &even_set));
        assert_eq!(true, odd_set.is_subset(&heap, &full_set));

        assert_eq!(false, even_set.is_subset(&heap, &empty_set));
        assert_eq!(false, even_set.is_subset(&heap, &odd_set));
        assert_eq!(true, even_set.is_subset(&heap, &even_set));
        assert_eq!(true, even_set.is_subset(&heap, &full_set));

        assert_eq!(false, full_set.is_subset(&heap, &empty_set));
        assert_eq!(false, full_set.is_subset(&heap, &odd_set));
        assert_eq!(false, full_set.is_subset(&heap, &even_set));
        assert_eq!(true, full_set.is_subset(&heap, &full_set));
    }
}
