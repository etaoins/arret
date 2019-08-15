use std::hash::{Hash, Hasher};
use std::{fmt, marker, mem};

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::boxed::*;

const MAX_16BYTE_INLINE_LENGTH: usize = ((16 - 8) / mem::size_of::<Gc<Any>>());
const MAX_32BYTE_INLINE_LENGTH: usize = ((32 - 8) / mem::size_of::<Gc<Any>>());
const MAX_INLINE_LENGTH: usize = MAX_32BYTE_INLINE_LENGTH;

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
    /// Constructs a new vector with the passed boxed values
    pub fn new(heap: &mut impl AsHeap, values: &[Gc<T>]) -> Gc<Vector<T>> {
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
        Self::new(heap, elems.as_slice())
    }

    fn is_inline(&self) -> bool {
        self.inline_length <= (MAX_INLINE_LENGTH as u32)
    }

    fn as_repr(&self) -> Repr<'_, T> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Vector<T> as *const InlineVector<T>) })
        } else {
            Repr::External(unsafe { &*(self as *const Vector<T> as *const ExternalVector<T>) })
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

    /// Returns an iterator over the vector
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &Gc<T>> {
        match self.as_repr() {
            Repr::Inline(inline) => inline.values[0..self.len()].iter(),
            Repr::External(external) => external.values.iter(),
        }
    }

    pub(crate) fn values_mut(&mut self) -> &mut [Gc<T>] {
        unsafe {
            if self.is_inline() {
                &mut (*(self as *mut Vector<T> as *mut InlineVector<T>)).values[0..self.len()]
            } else {
                &mut (*(self as *mut Vector<T> as *mut ExternalVector<T>)).values
            }
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
            .all(|(self_value, other_value)| self_value.eq_in_heap(heap, other_value))
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
    values: [Gc<T>; MAX_INLINE_LENGTH],
}

impl<T: Boxed> InlineVector<T> {
    fn new(header: Header, values: &[Gc<T>]) -> InlineVector<T> {
        unsafe {
            let mut inline_values = mem::MaybeUninit::<[Gc<T>; MAX_INLINE_LENGTH]>::uninit();
            ptr::copy(
                values.as_ptr(),
                inline_values.as_mut_ptr() as *mut _,
                values.len(),
            );

            InlineVector {
                header,
                inline_length: values.len() as u32,
                values: inline_values.assume_init(),
            }
        }
    }
}

#[repr(C, align(16))]
pub struct ExternalVector<T: Boxed> {
    header: Header,
    inline_length: u32,
    values: Vec<Gc<T>>,
}

impl<T: Boxed> ExternalVector<T> {
    fn new(header: Header, values: &[Gc<T>]) -> ExternalVector<T> {
        ExternalVector {
            header,
            inline_length: (MAX_INLINE_LENGTH + 1) as u32,
            values: values.into(),
        }
    }
}

enum Repr<'a, T: Boxed> {
    Inline(&'a InlineVector<T>),
    External(&'a ExternalVector<T>),
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

        let forward_vec1 = Vector::new(&mut heap, &[boxed1, boxed2, boxed3]);
        let forward_vec2 = Vector::new(&mut heap, &[boxed1, boxed2, boxed3]);
        let reverse_vec = Vector::new(&mut heap, &[boxed3, boxed2, boxed1]);

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
