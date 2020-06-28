use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use crate::abitype::{BoxedABIType, EncodeBoxedABIType};
use crate::boxed::refs::Gc;
use crate::boxed::*;

/// Immutable map of boxed values
#[repr(C, align(16))]
pub struct Map<K: Boxed = Any, V: Boxed = Any> {
    header: Header,
    _key: PhantomData<K>,
    _value: PhantomData<V>,
}

impl<K: Boxed, V: Boxed> Boxed for Map<K, V> {}

impl<K: Boxed, V: Boxed> Map<K, V> {
    /// Constructs a new map with the given values
    pub fn new(
        heap: &mut impl AsHeap,
        values: impl ExactSizeIterator<Item = (Gc<K>, Gc<V>)>,
    ) -> Gc<Map<K, V>> {
        if values.len() != 0 {
            todo!("non-empty maps");
        }

        heap.as_heap_mut().place_box(Map {
            header: Map::TYPE_TAG.to_heap_header(Self::size()),
            _key: PhantomData,
            _value: PhantomData,
        })
    }

    /// Returns the box size for maps
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }

    /// Return if the map is empty
    pub fn is_empty(&self) -> bool {
        true
    }

    /// Returns the number of the entries in the map
    pub fn len(&self) -> usize {
        0
    }

    /// Returns an iterator over the entries in map
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (Gc<K>, Gc<V>)> + 'a {
        std::iter::empty()
    }
}

impl<K: Boxed, V: Boxed> PartialEqInHeap for Map<K, V> {
    fn eq_in_heap(&self, _heap: &Heap, _other: &Map<K, V>) -> bool {
        // Both maps must be empty
        true
    }
}

impl<K: Boxed, V: Boxed> HashInHeap for Map<K, V> {
    fn hash_in_heap<H: Hasher>(&self, _heap: &Heap, state: &mut H) {
        TypeTag::Map.hash(state);
    }
}

impl<K: Boxed, V: Boxed> fmt::Debug for Map<K, V> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        formatter.write_str("Map(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<K: Boxed, V: Boxed> EncodeBoxedABIType for Map<K, V>
where
    K: EncodeBoxedABIType,
    V: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Map(&K::BOXED_ABI_TYPE, &V::BOXED_ABI_TYPE);
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(16, mem::size_of::<Map<Any>>());
    }
}
