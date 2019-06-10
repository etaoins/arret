use std::hash::{Hash, Hasher};
use std::{fmt, mem};

use crate::boxed::refs::Gc;
use crate::boxed::*;

/// Numeric ID indicating which class the record belongs to
///
/// This is used to distinguish record types before each other.
pub type RecordClassId = u32;

#[repr(C)]
struct RecordHeader {
    header: Header,
    inline_byte_length: u8,
    contains_boxes: bool,
    class_id: RecordClassId,
}

/// User-defined record type
#[repr(C, align(16))]
pub struct Record {
    record_header: RecordHeader,
    padding: [u8; Record::MAX_INLINE_BYTES],
}

impl Boxed for Record {}

impl Record {
    /// Maximum number of bytes that can be stored directly in the box
    pub const MAX_INLINE_BYTES: usize = 16;

    /// Constructs a new empty record of the given class
    pub fn new(heap: &mut impl AsHeap, class_id: RecordClassId) -> Gc<Record> {
        heap.as_heap_mut().place_box(Record {
            record_header: RecordHeader {
                header: Self::TYPE_TAG.to_heap_header(Self::size()),
                inline_byte_length: 0,
                contains_boxes: false,
                class_id,
            },
            padding: unsafe { mem::uninitialized() },
        })
    }

    /// Returns the box size for records
    pub fn size() -> BoxSize {
        BoxSize::Size16
    }

    /// Returns the class ID for the record
    pub fn class_id(&self) -> RecordClassId {
        self.record_header.class_id
    }

    /// Returns the record's data
    pub fn data(&self) -> &[u8] {
        match self.as_repr() {
            Repr::Inline(inline) => inline.as_slice(),
            Repr::Large(large) => large.external_data.as_ref(),
        }
    }

    fn is_inline(&self) -> bool {
        self.record_header.inline_byte_length <= Record::MAX_INLINE_BYTES as u8
    }

    fn as_repr<'a>(&'a self) -> Repr<'a> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Record as *const InlineRecord) })
        } else {
            Repr::Large(unsafe { &*(self as *const Record as *const LargeRecord) })
        }
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

#[repr(C, align(16))]
struct InlineRecord {
    record_header: RecordHeader,
    inline_data: [u8; Record::MAX_INLINE_BYTES],
}

impl InlineRecord {
    fn as_slice(&self) -> &[u8] {
        use std::slice;
        unsafe {
            slice::from_raw_parts(
                &self.inline_data[0] as *const u8,
                self.record_header.inline_byte_length as usize,
            )
        }
    }
}

#[repr(C, align(16))]
struct LargeRecord {
    record_header: RecordHeader,
    external_data: Box<[u8]>,
}

enum Repr<'a> {
    Inline(&'a InlineRecord),
    Large(&'a LargeRecord),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Record>());
        assert_eq!(32, mem::size_of::<InlineRecord>());
        assert!(mem::size_of::<LargeRecord>() <= 32);
    }

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
