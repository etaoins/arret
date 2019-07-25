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
    may_contain_gc_refs: bool,
    class_id: RecordClassId,
}

/// Describes the storage of a record's data
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RecordStorage {
    /// Record data is stored inline in a box of the given size
    Inline(BoxSize),
    /// Record data is stored out-of-line in a 32 byte box
    External,
}

impl RecordStorage {
    /// Returns the box size for a record storage
    pub fn box_size(self) -> BoxSize {
        match self {
            RecordStorage::Inline(box_size) => box_size,
            RecordStorage::External => BoxSize::Size32,
        }
    }
}

/// User-defined record type
#[repr(C, align(16))]
pub struct Record {
    record_header: RecordHeader,
    padding: [u8; Record::MAX_INLINE_BYTES],
}

impl Boxed for Record {}

impl Record {
    /// Maximum number of bytes that can be stored directly in a box
    pub const MAX_INLINE_BYTES: usize = 24;

    /// Alignment of our inline record data in bytes
    const INLINE_DATA_ALIGNMENT: usize = 8;

    /// Constructs a new record of the given class and initialises it with the passed data
    pub fn new(heap: &mut impl AsHeap, class_id: RecordClassId, data: RecordData) -> Gc<Record> {
        let storage = Self::storage_for_data_layout(data.layout());
        let box_size = storage.box_size();

        let boxed = unsafe {
            match storage {
                RecordStorage::External => {
                    mem::transmute(ExternalRecord::new(box_size, class_id, data))
                }
                RecordStorage::Inline(_) => {
                    mem::transmute(InlineRecord::new(box_size, class_id, data))
                }
            }
        };

        heap.as_heap_mut().place_box(boxed)
    }

    /// Returns the storage for given data layout
    pub fn storage_for_data_layout(data_layout: alloc::Layout) -> RecordStorage {
        if data_layout.align() > Self::INLINE_DATA_ALIGNMENT {
            // Requires more alignment than our inline data provides
            return RecordStorage::External;
        }

        match data_layout.size() {
            0..=8 => RecordStorage::Inline(BoxSize::Size16),
            9..=Record::MAX_INLINE_BYTES => RecordStorage::Inline(BoxSize::Size32),
            _ => RecordStorage::External,
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
        if !self.record_header.may_contain_gc_refs {
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
            Repr::External(external) => external.external_data.as_ptr(),
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
            Repr::External(unsafe { &*(self as *const Record as *const ExternalRecord) })
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
    fn new(box_size: BoxSize, class_id: RecordClassId, data: RecordData) -> InlineRecord {
        let header = Record::TYPE_TAG.to_heap_header(box_size);

        unsafe {
            let mut inline_data = mem::MaybeUninit::<[u8; Record::MAX_INLINE_BYTES]>::uninit();
            ptr::copy(
                data.as_ptr(),
                inline_data.as_mut_ptr() as *mut _,
                data.layout().size(),
            );

            InlineRecord {
                record_header: RecordHeader {
                    header,
                    inline_byte_len: data.layout().size() as u8,
                    // This is conservative - we don't know if there are GC refs or not
                    may_contain_gc_refs: (data.layout().size() > 0) as bool,
                    class_id,
                },
                inline_data: inline_data.assume_init(),
            }
        }
    }
}

#[repr(C, align(16))]
struct ExternalRecord {
    record_header: RecordHeader,
    external_data: RecordData,
}

impl ExternalRecord {
    fn new(box_size: BoxSize, class_id: RecordClassId, data: RecordData) -> ExternalRecord {
        let header = Record::TYPE_TAG.to_heap_header(box_size);

        ExternalRecord {
            record_header: RecordHeader {
                header,
                inline_byte_len: std::u8::MAX,
                // This is conservative - we don't know if there are GC refs or not
                may_contain_gc_refs: true,
                class_id,
            },

            external_data: data,
        }
    }
}

enum Repr<'a> {
    Inline(&'a InlineRecord),
    External(&'a ExternalRecord),
}

impl Drop for Record {
    fn drop(&mut self) {
        match self.as_repr() {
            Repr::Inline(_) => {
                // Do nothing here; we might've been allocated as a 16 byte box so we can't read
                // the whole thing.
            }
            Repr::External(external) => unsafe {
                // Call `ExternalRecord`'s drop implementation
                ptr::read(external);
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
        assert_eq!(32, mem::size_of::<ExternalRecord>());
    }

    #[test]
    fn equality() {
        let mut heap = Heap::empty();

        let record_class_one_instance1 = Record::new(&mut heap, 1, RecordData::empty());
        let record_class_one_instance2 = Record::new(&mut heap, 1, RecordData::empty());
        let record_class_two_instance1 = Record::new(&mut heap, 2, RecordData::empty());

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

        let boxed_one = Record::new(&mut heap, 1, RecordData::empty());
        assert_eq!("Record(1)", format!("{:?}", boxed_one));
    }
}
