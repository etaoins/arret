use std::alloc;
use std::hash::{Hash, Hasher};
use std::{fmt, mem};

use crate::boxed::refs::Gc;
use crate::boxed::types::field_value::FieldGcRefIter;
use crate::boxed::*;

/// Numeric ID indicating which class the record belongs to
///
/// This is used to distinguish record types before each other.
pub type RecordClassId = u32;

#[repr(C)]
struct RecordHeader {
    header: Header,
    inline_byte_len: u8,
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
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RecordStorage {
    /// Record data is stored inline in a box of the given size
    Inline(BoxSize),
    /// Record data is stored out-of-line in a 32 byte box
    Large,
}

impl RecordStorage {
    /// Returns the box size for a record layout
    pub fn box_size(self) -> BoxSize {
        match self {
            RecordStorage::Inline(box_size) => box_size,
            RecordStorage::Large => BoxSize::Size32,
        }
    }
}

impl Boxed for Record {}

impl Record {
    /// Maximum number of bytes that can be stored directly in a box
    pub const MAX_INLINE_BYTES: usize = 24;

    /// Constructs a new empty record of the given class
    pub fn new(heap: &mut impl AsHeap, class_id: RecordClassId, data: &[u8]) -> Gc<Record> {
        let storage = Self::storage_for_data_len(data.len());

        let box_size = storage.box_size();
        let boxed = unsafe {
            match storage {
                RecordStorage::Large => mem::transmute(LargeRecord::new(box_size, class_id, data)),
                RecordStorage::Inline(_) => {
                    mem::transmute(InlineRecord::new(box_size, class_id, data))
                }
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    /// Returns the storage for given data length
    pub fn storage_for_data_len(data_len: usize) -> RecordStorage {
        match data_len {
            0..=8 => RecordStorage::Inline(BoxSize::Size16),
            9..=Record::MAX_INLINE_BYTES => RecordStorage::Inline(BoxSize::Size32),
            _ => RecordStorage::Large,
        }
    }

    /// Returns the class ID for the record
    pub fn class_id(&self) -> RecordClassId {
        self.record_header.class_id
    }

    /// Returns an iterator over the record's field values
    pub fn field_values<'cm>(&self, heap: &'cm Heap) -> FieldValueIter<'cm> {
        let classmap_class = heap
            .type_info()
            .class_map()
            .class_for_record_class_id(self.class_id());

        FieldValueIter {
            classmap_field_iter: classmap_class.field_iter(),
            record_data: self.data_ptr(),
        }
    }

    pub(crate) fn field_gc_refs<'cm>(&mut self, heap: &'cm Heap) -> FieldGcRefIter<'cm> {
        if !self.record_header.contains_gc_refs {
            return FieldGcRefIter::empty();
        }

        let classmap_class = heap
            .type_info()
            .class_map()
            .class_for_record_class_id(self.class_id());

        FieldGcRefIter {
            classmap_field_iter: classmap_class.field_iter(),
            record_data: self.data_ptr(),
        }
    }

    fn data_ptr(&self) -> *const u8 {
        match self.as_repr() {
            Repr::Inline(inline) => &inline.inline_data[0] as *const u8,
            Repr::Large(large) => large.large_data,
        }
    }

    fn is_empty(&self) -> bool {
        self.record_header.inline_byte_len == 0
    }

    fn is_inline(&self) -> bool {
        self.record_header.inline_byte_len <= Self::MAX_INLINE_BYTES as u8
    }

    fn as_repr(&self) -> Repr<'_> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Record as *const InlineRecord) })
        } else {
            Repr::Large(unsafe { &*(self as *const Record as *const LargeRecord) })
        }
    }
}

impl PartialEqInHeap for Record {
    fn eq_in_heap(&self, heap: &Heap, other: &Record) -> bool {
        if self.class_id() != other.class_id() {
            return false;
        }

        if self.is_empty() {
            return true;
        }

        self.field_values(heap)
            .zip(other.field_values(heap))
            .all(|(self_field, other_field)| self_field.eq_in_heap(heap, &other_field))
    }
}

impl HashInHeap for Record {
    fn hash_in_heap<H: Hasher>(&self, heap: &Heap, state: &mut H) {
        Self::TYPE_TAG.hash(state);
        self.class_id().hash(state);

        if !self.is_empty() {
            for field in self.field_values(heap) {
                field.hash_in_heap(heap, state);
            }
        }
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
    pub fn new(box_size: BoxSize, class_id: RecordClassId, data: &[u8]) -> InlineRecord {
        let header = Record::TYPE_TAG.to_heap_header(box_size);

        unsafe {
            let mut inline_record = InlineRecord {
                record_header: RecordHeader {
                    header,
                    inline_byte_len: data.len() as u8,
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

            inline_record
        }
    }
}

#[repr(C, align(16))]
struct LargeRecord {
    record_header: RecordHeader,
    large_data: *const u8,
    large_byte_len: usize,
}

impl LargeRecord {
    pub fn new(box_size: BoxSize, class_id: RecordClassId, data: &[u8]) -> LargeRecord {
        unsafe {
            let header = Record::TYPE_TAG.to_heap_header(box_size);
            let alloc_layout = Self::alloc_layout_for_byte_len(data.len());
            let large_data = alloc::alloc(alloc_layout);

            ptr::copy(data.as_ptr(), large_data as *mut u8, data.len());

            LargeRecord {
                record_header: RecordHeader {
                    header,
                    inline_byte_len: std::u8::MAX,
                    contains_gc_refs: false,
                    class_id,
                },

                large_data,
                large_byte_len: data.len(),
            }
        }
    }

    fn alloc_layout_for_byte_len(byte_len: usize) -> alloc::Layout {
        unsafe { alloc::Layout::from_size_align_unchecked(byte_len, 8) }
    }
}

impl Drop for LargeRecord {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(
                self.large_data as *mut u8,
                Self::alloc_layout_for_byte_len(self.large_byte_len),
            );
        }
    }
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
    fn fmt_debug() {
        let mut heap = Heap::empty();

        let boxed_one = Record::new(&mut heap, 1, &[]);
        assert_eq!("Record(1)", format!("{:?}", boxed_one));
    }
}
