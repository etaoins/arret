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
    contains_gc_refs: bool,
    class_id: RecordClassId,
}

/// User-defined record type
#[repr(C, align(16))]
pub struct Record {
    record_header: RecordHeader,
    padding: [u8; Record::MAX_INLINE_BYTES],
}

/// Describes the storage of a record's data
#[derive(Clone, Copy, Debug)]
pub enum RecordStorage {
    /// Record data is stored inline in a box of the given size
    Inline(BoxSize),
    /// Record data is stored out-of-line in a box of the given size
    Large(BoxSize),
}

impl RecordStorage {
    /// Returns the box size for a record layout
    pub fn box_size(&self) -> BoxSize {
        match self {
            RecordStorage::Inline(box_size) => *box_size,
            RecordStorage::Large(box_size) => *box_size,
        }
    }
}

impl Boxed for Record {}

impl Record {
    /// Maximum number of bytes that can be stored directly in a box
    pub const MAX_INLINE_BYTES: usize = 24;

    /// Constructs a new empty record of the given class
    pub fn new(heap: &mut impl AsHeap, class_id: RecordClassId, data: &[u8]) -> Gc<Record> {
        let storage = Self::storage_for_data_len(data.len(), mem::size_of::<usize>() * 8);

        let box_size = storage.box_size();
        let header = Self::TYPE_TAG.to_heap_header(box_size);

        let boxed = unsafe {
            match storage {
                RecordStorage::Large(_) => {
                    let large_record = LargeRecord {
                        record_header: RecordHeader {
                            header,
                            inline_byte_length: std::u8::MAX,
                            contains_gc_refs: false,
                            class_id,
                        },
                        external_data: data.into(),
                        #[cfg(target_pointer_width = "32")]
                        padding: 0,
                    };

                    mem::transmute(large_record)
                }
                RecordStorage::Inline(_) => {
                    let mut inline_record = InlineRecord {
                        record_header: RecordHeader {
                            header,
                            inline_byte_length: data.len() as u8,
                            contains_gc_refs: false,
                            class_id,
                        },
                        inline_data: mem::uninitialized(),
                    };

                    ptr::copy(
                        data.as_ptr(),
                        &mut inline_record.inline_data[0] as *mut u8,
                        data.len(),
                    );

                    mem::transmute(inline_record)
                }
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    /// Returns the storage for given data length and target pointer width in bits
    pub fn storage_for_data_len(data_len: usize, pointer_width: usize) -> RecordStorage {
        match data_len {
            0..=8 => RecordStorage::Inline(BoxSize::Size16),
            9..=Record::MAX_INLINE_BYTES => RecordStorage::Inline(BoxSize::Size32),
            _ => RecordStorage::Large(match pointer_width {
                32 => BoxSize::Size16,
                64 => BoxSize::Size32,
                other => panic!("unsupported pointer width: {}", other),
            }),
        }
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

    /// Returns true if this record contains zero-length data
    pub fn is_empty(&self) -> bool {
        self.record_header.inline_byte_length == 0
    }

    /// Returns true if this record contains pointers to garbage collected boxes
    pub(crate) fn contains_gc_refs(&self) -> bool {
        self.record_header.contains_gc_refs
    }

    fn is_inline(&self) -> bool {
        self.record_header.inline_byte_length <= Self::MAX_INLINE_BYTES as u8
    }

    fn as_repr<'a>(&'a self) -> Repr<'a> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Record as *const InlineRecord) })
        } else {
            Repr::Large(unsafe { &*(self as *const Record as *const LargeRecord) })
        }
    }
}

impl PartialEqInHeap for Record {
    fn eq_in_heap(&self, _heap: &Heap, other: &Record) -> bool {
        if !self.is_empty() {
            unimplemented!("equality between non-empty records");
        }

        self.class_id() == other.class_id()
    }
}

impl HashInHeap for Record {
    fn hash_in_heap<H: Hasher>(&self, _heap: &Heap, state: &mut H) {
        if !self.is_empty() {
            unimplemented!("hash for non-empty records");
        }

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

    #[cfg(target_pointer_width = "32")]
    padding: u64,
}

enum Repr<'a> {
    Inline(&'a InlineRecord),
    Large(&'a LargeRecord),
}

impl Drop for Record {
    fn drop(&mut self) {
        // ptr::read will properly drop our specific representations
        match self.as_repr() {
            Repr::Inline(inline) => unsafe {
                ptr::read(inline);
            },
            Repr::Large(large) => unsafe {
                ptr::read(large);
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::boxed::heap::Heap;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Record>());
        assert_eq!(32, mem::size_of::<InlineRecord>());
        assert_eq!(32, mem::size_of::<LargeRecord>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let record_class_one_instance1 = Record::new(&mut heap, 1, &[]);
        let record_class_one_instance2 = Record::new(&mut heap, 1, &[]);

        let record_class_two_instance1 = Record::new(&mut heap, 2, &[]);

        assert_eq!(
            true,
            record_class_one_instance1.eq_in_heap(&heap, &record_class_one_instance2)
        );

        assert_eq!(
            false,
            record_class_one_instance1.eq_in_heap(&heap, &record_class_two_instance1)
        );
    }

    #[test]
    fn data_round_trip() {
        let mut heap = Heap::empty();

        // Allocate these all upfront so if we misjudge a box size we'll stomp over ourselves
        let empty_record = Record::new(&mut heap, 1, b"");
        let one_cell_inline_record = Record::new(&mut heap, 2, b"hello");
        let two_cell_inline_record = Record::new(&mut heap, 3, b"Hello, world!");
        let large_record = Record::new(&mut heap, 4, b"This is a very large record data");

        assert_eq!(true, empty_record.is_empty());
        assert_eq!(true, empty_record.is_inline());
        assert_eq!(b"", empty_record.data());

        assert_eq!(false, one_cell_inline_record.is_empty());
        assert_eq!(true, one_cell_inline_record.is_inline());
        assert_eq!(b"hello", one_cell_inline_record.data());

        assert_eq!(false, two_cell_inline_record.is_empty());
        assert_eq!(true, two_cell_inline_record.is_inline());
        assert_eq!(b"Hello, world!", two_cell_inline_record.data());

        assert_eq!(false, large_record.is_empty());
        assert_eq!(false, large_record.is_inline());
        assert_eq!(b"This is a very large record data", large_record.data());
    }

    #[test]
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Record::new(&mut heap, 1, &[]);
        assert_eq!("Record(1)", format!("{:?}", boxed_one));
    }
}
