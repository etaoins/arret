use std::marker::PhantomData;
use std::ptr;

use crate::abitype;
use crate::boxed::RecordClassId;

/// Minimal type information for a class' field
///
/// This is used to annotate values with information to support equality, hashing and garbage
/// collection.
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum FieldType {
    Bool = 0,
    Char = 1,
    Float = 2,
    Int = 3,
    InternedSym = 4,
    Boxed = 5,
    Callback = 6,
}

impl FieldType {
    pub fn from_abi_type(abi_type: &abitype::ABIType) -> Self {
        match abi_type {
            abitype::ABIType::Bool => FieldType::Bool,
            abitype::ABIType::Char => FieldType::Char,
            abitype::ABIType::Float => FieldType::Float,
            abitype::ABIType::Int => FieldType::Int,
            abitype::ABIType::InternedSym => FieldType::InternedSym,
            abitype::ABIType::Boxed(_) => FieldType::Boxed,
            abitype::ABIType::Callback(_) => FieldType::Callback,
        }
    }
}

/// Type information for a class' field
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Field {
    offset: u32,
    field_type: FieldType,
    is_last: bool,
}

impl Field {
    /// Constructs a new instance with the given field type and offset
    pub fn new(field_type: FieldType, offset: usize) -> Field {
        Self {
            offset: offset as u32,
            field_type,
            is_last: false,
        }
    }

    /// Type information for the class field
    pub fn field_type(self) -> FieldType {
        self.field_type
    }

    /// Offset in bytes of the class field from the start of the record data
    pub fn offset(self) -> usize {
        self.offset as usize
    }

    /// Returns if this is the last field in the class
    pub fn is_last(self) -> bool {
        self.is_last
    }
}

/// Type information for a class
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ClassRef<'a> {
    fields: *const Field,
    phantom_lifetime: PhantomData<&'a Field>,
}

impl<'a> ClassRef<'a> {
    /// Returns if the type contains no fields
    pub fn is_empty(self) -> bool {
        self.fields.is_null()
    }

    /// Returns an iterator over the class' fields
    pub fn field_iter(self) -> FieldIterator<'a> {
        FieldIterator {
            fields: self.fields,
            phantom_lifetime: PhantomData,
        }
    }
}

/// Owned version of [`ClassRef`]
///
/// This is used for classes that are built during compile time.
#[derive(Clone)]
pub struct BoxedClass {
    fields: Box<[Field]>,
}

impl BoxedClass {
    /// Constructs a new instance containing the provided fields
    pub fn from_fields(fields_iter: impl Iterator<Item = Field>) -> Self {
        let mut fields: Box<[Field]> = fields_iter.collect();
        if let Some(last_field) = fields.last_mut() {
            last_field.is_last = true;
        }

        BoxedClass { fields }
    }

    pub fn as_ref(&self) -> ClassRef<'_> {
        ClassRef {
            fields: if self.fields.is_empty() {
                std::ptr::null()
            } else {
                self.fields.as_ptr()
            },
            phantom_lifetime: PhantomData,
        }
    }
}

/// Basic iterator of class fields
pub struct FieldIterator<'a> {
    fields: *const Field,
    phantom_lifetime: PhantomData<&'a Field>,
}

impl<'a> Iterator for FieldIterator<'a> {
    type Item = Field;

    fn next(&mut self) -> Option<Field> {
        if self.fields.is_null() {
            return None;
        }

        let next_field = unsafe { *self.fields };

        if next_field.is_last {
            self.fields = ptr::null();
        } else {
            self.fields = unsafe { self.fields.add(1) };
        }

        Some(next_field)
    }
}

/// Mapping of [record class IDs](RecordClassId) to [classes](ClassRef)
#[repr(C)]
#[derive(Clone)]
pub struct ClassMap {
    const_classes: *const ClassRef<'static>,
    dynamic_classes: Vec<BoxedClass>,
}

impl ClassMap {
    const DYNAMIC_RECORD_CLASS_ID_BASE: RecordClassId = 1u32 << 30;

    /// Constructs a new instance containing no classes
    pub fn empty() -> ClassMap {
        Self::with_const_classes(std::ptr::null())
    }

    /// Constructs a new instance with the provided constant classes
    pub(crate) fn with_const_classes(const_classes: *const ClassRef<'static>) -> ClassMap {
        Self {
            const_classes,
            dynamic_classes: vec![],
        }
    }

    /// Registers a new class and returns a distinct [`RecordClassId`]
    pub fn push_dynamic_class(&mut self, boxed_class: BoxedClass) -> RecordClassId {
        let record_class_id =
            (self.dynamic_classes.len() as RecordClassId) + Self::DYNAMIC_RECORD_CLASS_ID_BASE;

        self.dynamic_classes.push(boxed_class);
        record_class_id
    }

    /// Returns a class reference for a given [`RecordClassId`]
    pub fn class_for_record_class_id(&self, record_class_id: RecordClassId) -> ClassRef<'_> {
        if record_class_id >= Self::DYNAMIC_RECORD_CLASS_ID_BASE {
            let dynamic_class_index =
                (record_class_id - Self::DYNAMIC_RECORD_CLASS_ID_BASE) as usize;

            self.dynamic_classes[dynamic_class_index].as_ref()
        } else {
            unsafe { *self.const_classes.add(record_class_id as usize) }
        }
    }
}
