use std::hash::{Hash, Hasher};
use std::{fmt, marker, mem};

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::boxed::*;
use crate::persistent::Vector as PersistentVector;

const MAX_16BYTE_INLINE_LENGTH: usize = (16 - 8) / mem::size_of::<Gc<Any>>();
const MAX_32BYTE_INLINE_LENGTH: usize = (32 - 8) / mem::size_of::<Gc<Any>>();

/// Describes the storage of a vector's data
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VectorStorage {
    /// Vector data is stored inline in a box of the given size
    Inline(BoxSize),
    /// Vector data is stored out-of-line in a 32 byte box
    External,
}

impl VectorStorage {
    /// Returns the box size for a vector storage
    pub fn box_size(self) -> BoxSize {
        match self {
            VectorStorage::Inline(box_size) => box_size,
            VectorStorage::External => BoxSize::Size32,
        }
    }
}

/// Immutable vector of boxed values
///
/// This allows random access to any of its values.
#[repr(C, align(16))]
pub struct Vector<T: Boxed = Any> {
    header: Header,
    inline_length: u32,
    padding: [u8; 24],
    phantom: marker::PhantomData<T>,
}

impl<T: Boxed> Boxed for Vector<T> {}

impl<T: Boxed> Vector<T> {
    /// Maximum element length of an inline vector
    pub const MAX_INLINE_LENGTH: usize = MAX_32BYTE_INLINE_LENGTH;

    /// Constructs a new vector with the passed boxed values
    pub fn new(
        heap: &mut impl AsHeap,
        values: impl ExactSizeIterator<Item = Gc<T>>,
    ) -> Gc<Vector<T>> {
        let storage = Self::storage_for_element_len(values.len());
        let header = Vector::TYPE_TAG.to_heap_header(storage.box_size());

        let boxed = unsafe {
            match storage {
                VectorStorage::External => mem::transmute(ExternalVector::new(header, values)),
                VectorStorage::Inline(_) => mem::transmute(InlineVector::new(header, values)),
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    /// Returns the storage for given element length
    fn storage_for_element_len(len: usize) -> VectorStorage {
        const MIN_32BYTE_INLINE_LENGTH: usize = MAX_16BYTE_INLINE_LENGTH + 1;

        match len {
            0..=MAX_16BYTE_INLINE_LENGTH => VectorStorage::Inline(BoxSize::Size16),
            MIN_32BYTE_INLINE_LENGTH..=MAX_32BYTE_INLINE_LENGTH => {
                VectorStorage::Inline(BoxSize::Size32)
            }
            _ => {
                // Too big to fit inline; this needs to be external
                VectorStorage::External
            }
        }
    }

    /// Constructs a vector by constructing an iterator of values
    pub fn from_values<V, F>(
        heap: &mut impl AsHeap,
        values: impl Iterator<Item = V>,
        cons: F,
    ) -> Gc<Vector<T>>
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
            Repr::Inline(unsafe { &*(self as *const Vector<T> as *const InlineVector<T>) })
        } else {
            Repr::External(unsafe { &*(self as *const Vector<T> as *const ExternalVector<T>) })
        }
    }

    fn as_repr_mut(&mut self) -> ReprMut<'_, T> {
        if self.is_inline() {
            ReprMut::Inline(unsafe { &mut *(self as *mut Vector<T> as *mut InlineVector<T>) })
        } else {
            ReprMut::External(unsafe { &mut *(self as *mut Vector<T> as *mut ExternalVector<T>) })
        }
    }

    /// Returns the length of the vector
    pub fn len(&self) -> usize {
        match self.as_repr() {
            Repr::Inline(inline) => inline.inline_length as usize,
            Repr::External(external) => external.values.len(),
        }
    }

    /// Returns true if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.inline_length == 0
    }

    /// Return an element as the provided index
    pub fn get(&self, index: usize) -> Option<Gc<T>> {
        match self.as_repr() {
            Repr::Inline(inline) => inline.get(index).copied(),
            Repr::External(external) => external.values.get(index),
        }
    }

    /// Returns an iterator over the vector
    pub fn iter<'a>(&'a self) -> Box<(dyn ExactSizeIterator<Item = Gc<T>> + 'a)> {
        match self.as_repr() {
            Repr::Inline(inline) => Box::new(inline.iter()),
            Repr::External(external) => Box::new(external.values.iter()),
        }
    }

    /// Returns a new vector with the element at the given index replaced
    pub fn assoc(&self, heap: &mut impl AsHeap, index: usize, value: Gc<T>) -> Gc<Vector<T>> {
        match self.as_repr() {
            Repr::Inline(inline) => {
                let mut values = inline.values;

                values[index] = value;
                Vector::new(heap, values[0..self.len()].iter().copied())
            }
            Repr::External(external) => {
                let boxed = unsafe {
                    mem::transmute(ExternalVector {
                        header: external.header,
                        inline_length: external.inline_length,
                        values: external.values.assoc(index, value),
                    })
                };

                heap.as_heap_mut().place_box(boxed)
            }
        }
    }

    /// Returns a new vector with the element appended
    pub fn push(&self, heap: &mut impl AsHeap, value: Gc<T>) -> Gc<Vector<T>> {
        match self.as_repr() {
            Repr::Inline(inline) => {
                const MAX_INLINE_LENGTH_PLUS_ONE: usize = Vector::<Any>::MAX_INLINE_LENGTH + 1;

                let mut values: [Gc<T>; MAX_INLINE_LENGTH_PLUS_ONE] =
                    [unsafe { Gc::new(ptr::null()) }; MAX_INLINE_LENGTH_PLUS_ONE];

                (&mut values[0..self.len()]).copy_from_slice(&inline.values[0..self.len()]);
                values[self.len()] = value;

                Vector::new(heap, values[0..self.len() + 1].iter().copied())
            }
            Repr::External(external) => {
                let boxed = unsafe {
                    mem::transmute(ExternalVector {
                        header: external.header,
                        inline_length: external.inline_length,
                        values: external.values.push(value),
                    })
                };

                heap.as_heap_mut().place_box(boxed)
            }
        }
    }

    /// Returns a new vector without its final element
    pub fn pop(&self, heap: &mut impl AsHeap) -> Option<(Gc<Vector<T>>, Gc<T>)> {
        if self.is_empty() {
            return None;
        }

        match self.as_repr() {
            Repr::Inline(inline) => {
                const MAX_INLINE_LENGTH_MINUS_ONE: usize = Vector::<Any>::MAX_INLINE_LENGTH + 1;

                let mut values: [Gc<T>; MAX_INLINE_LENGTH_MINUS_ONE] =
                    [unsafe { Gc::new(ptr::null()) }; MAX_INLINE_LENGTH_MINUS_ONE];

                let new_len = self.len() - 1;
                (&mut values[0..new_len]).copy_from_slice(&inline.values[0..new_len]);

                let new_vector = Vector::new(heap, values[0..new_len].iter().copied());
                Some((new_vector, inline.values[self.len() - 1]))
            }
            Repr::External(external) => {
                let (values, element) = external.values.pop().unwrap();

                let boxed = unsafe {
                    mem::transmute(ExternalVector {
                        header: external.header,
                        inline_length: external.inline_length,
                        values,
                    })
                };

                Some((heap.as_heap_mut().place_box(boxed), element))
            }
        }
    }

    pub(crate) fn visit_mut_elements<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut Gc<T>),
    {
        match self.as_repr_mut() {
            ReprMut::Inline(inline) => {
                for element in inline.iter_mut() {
                    visitor(element);
                }
            }
            ReprMut::External(external) => external.values.visit_mut_elements(visitor),
        }
    }
}

impl<T: Boxed> PartialEqInHeap for Vector<T> {
    fn eq_in_heap(&self, heap: &Heap, other: &Vector<T>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.iter()
            .zip(other.iter())
            .all(|(self_value, other_value)| self_value.eq_in_heap(heap, &other_value))
    }
}

impl<T: Boxed> HashInHeap for Vector<T> {
    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
        TypeTag::Vector.hash(state);
        state.write_usize(self.len());
        for value in self.iter() {
            value.hash_in_heap(heap, state);
        }
    }
}

impl<T: Boxed> fmt::Debug for Vector<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        formatter.write_str("Vector(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<T: Boxed> EncodeBoxedABIType for Vector<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Vector(&T::BOXED_ABI_TYPE);
}

#[repr(C, align(16))]
pub struct InlineVector<T: Boxed> {
    header: Header,
    inline_length: u32,
    values: [Gc<T>; MAX_32BYTE_INLINE_LENGTH],
}

impl<T: Boxed> InlineVector<T> {
    fn new(header: Header, values: impl ExactSizeIterator<Item = Gc<T>>) -> InlineVector<T> {
        unsafe {
            let inline_length = values.len();

            let mut inline_values =
                mem::MaybeUninit::<[Gc<T>; MAX_32BYTE_INLINE_LENGTH]>::uninit().assume_init();

            for (inline_value, value) in inline_values.iter_mut().zip(values) {
                ptr::write(inline_value, value);
            }

            InlineVector {
                header,
                inline_length: inline_length as u32,
                values: inline_values,
            }
        }
    }

    fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = Gc<T>> + 'a {
        self.values[0..self.inline_length as usize].iter().copied()
    }

    fn iter_mut<'a>(&'a mut self) -> impl ExactSizeIterator<Item = &mut Gc<T>> + 'a {
        self.values[0..self.inline_length as usize].iter_mut()
    }

    fn get(&self, index: usize) -> Option<&Gc<T>> {
        if index > self.inline_length as usize {
            None
        } else {
            Some(&self.values[index])
        }
    }
}

#[repr(C, align(16))]
pub struct ExternalVector<T: Boxed> {
    header: Header,
    inline_length: u32,
    values: PersistentVector<Gc<T>>,
}

impl<T: Boxed> ExternalVector<T> {
    fn new(header: Header, values: impl ExactSizeIterator<Item = Gc<T>>) -> ExternalVector<T> {
        ExternalVector {
            header,
            inline_length: (Vector::<T>::MAX_INLINE_LENGTH + 1) as u32,
            values: PersistentVector::new(values),
        }
    }
}

enum Repr<'a, T: Boxed> {
    Inline(&'a InlineVector<T>),
    External(&'a ExternalVector<T>),
}

enum ReprMut<'a, T: Boxed> {
    Inline(&'a mut InlineVector<T>),
    External(&'a mut ExternalVector<T>),
}

impl<T: Boxed> Drop for Vector<T> {
    fn drop(&mut self) {
        match self.as_repr() {
            Repr::Inline(_) => {
                // Do nothing here; we might've been allocated as a 16 byte box so we can't read
                // the whole thing.
            }
            Repr::External(external) => unsafe {
                // Call `ExternalVector`'s drop implementation
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
        assert_eq!(32, mem::size_of::<Vector<Any>>());
        assert_eq!(32, mem::size_of::<InlineVector<Any>>());
        assert_eq!(32, mem::size_of::<ExternalVector<Any>>());
    }

    #[test]
    fn equality() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);

        let forward_vec1 = Vector::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());
        let forward_vec2 = Vector::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());
        let reverse_vec = Vector::new(&mut heap, vec![boxed3, boxed2, boxed1].into_iter());

        assert_eq!(false, forward_vec1.eq_in_heap(&heap, &reverse_vec));
        assert_eq!(true, forward_vec1.eq_in_heap(&heap, &forward_vec2));
    }

    #[test]
    fn fmt_debug() {
        use crate::boxed::Int;

        let mut heap = Heap::empty();

        let forward_vec = Vector::from_values(&mut heap, [1, 2, 3].iter().cloned(), Int::new);

        assert_eq!(
            "Vector([Int(1), Int(2), Int(3)])",
            format!("{:?}", forward_vec)
        );
    }
}
