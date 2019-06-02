use std::fmt;
use std::hash::{Hash, Hasher};

use crate::boxed::refs::Gc;
use crate::boxed::*;

/// Numeric ID indicating which class the record belongs to
///
/// This is used to distinguish record types before each other.
pub type ClassId = u32;

/// User-defined record type
///
/// TODO: This only supports empty records
#[repr(C, align(16))]
pub struct Record {
    header: Header,
    class_id: ClassId,
}

impl Boxed for Record {}

impl Record {
    /// Constructs a new empty record of the given class
    pub fn new(heap: &mut impl AsHeap, class_id: ClassId) -> Gc<Record> {
        heap.as_heap_mut().place_box(Record {
            header: Self::TYPE_TAG.to_heap_header(Self::size()),
            class_id,
        })
    }

    /// Returns the box size for integers
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }

    /// Returns the class ID for the record
    pub fn class_id(&self) -> ClassId {
        self.class_id
    }
}

impl PartialEq for Record {
    fn eq(&self, other: &Record) -> bool {
        self.class_id() == other.class_id()
    }
}

impl Hash for Record {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        self.class_id().hash(state)
    }
}

impl fmt::Debug for Record {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Record({:?})", self.class_id())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let record_class_one_instance1 = Record::new(&mut heap, 1);
        let record_class_one_instance2 = Record::new(&mut heap, 1);

        let record_class_two_instance1 = Record::new(&mut heap, 2);

        assert_eq!(record_class_one_instance1, record_class_one_instance2);
        assert_ne!(record_class_one_instance1, record_class_two_instance1);
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Record::new(&mut heap, 1);
        assert_eq!("Record(1)", format!("{:?}", boxed_one));
    }
}
