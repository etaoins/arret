use std::hash::{Hash, Hasher};
use std::ptr;

use crate::boxed;
use crate::boxed::prelude::*;
use crate::boxed::refs::Gc;
use crate::class_map;
use crate::intern::InternedSym;

/// Field within a record value
pub enum FieldValue {
    /// Unboxed boolean
    Bool(bool),
    /// Unboxed Unicode character
    Char(char),
    /// Unboxed 64bit floating point value
    Float(f64),
    /// Unboxed 64bit signed integer
    Int(i64),
    /// Interned symbol
    InternedSym(InternedSym),
    /// Boxed garbage collected value
    Boxed(Gc<boxed::Any>),
    /// Callback function of an unknown type
    Callback,
}

pub(crate) enum FieldGcRef {
    InternedSym(&'static mut InternedSym),
    Boxed(&'static mut Gc<boxed::Any>),
}

impl PartialEqInHeap for FieldValue {
    fn eq_in_heap(&self, heap: &boxed::Heap, other: &FieldValue) -> bool {
        match (self, other) {
            (FieldValue::Bool(sv), FieldValue::Bool(ov)) => sv == ov,
            (FieldValue::Char(sv), FieldValue::Char(ov)) => sv == ov,
            (FieldValue::Float(sv), FieldValue::Float(ov)) => sv == ov,
            (FieldValue::Int(sv), FieldValue::Int(ov)) => sv == ov,
            (FieldValue::InternedSym(sv), FieldValue::InternedSym(ov)) => sv == ov,
            (FieldValue::Boxed(sv), FieldValue::Boxed(ov)) => sv.eq_in_heap(heap, ov),
            (FieldValue::Callback, FieldValue::Callback) => false,
            _ => false,
        }
    }
}

impl HashInHeap for FieldValue {
    fn hash_in_heap<H: Hasher>(&self, heap: &boxed::Heap, state: &mut H) {
        match self {
            FieldValue::Bool(v) => (*v).hash(state),
            FieldValue::Char(v) => (*v).hash(state),
            FieldValue::Float(v) => {
                // See `boxed::Float::hash`
                if *v == 0.0 {
                    state.write_u64((0.0f64).to_bits())
                } else {
                    state.write_u64(v.to_bits());
                }
            }
            FieldValue::Int(v) => (*v).hash(state),
            FieldValue::InternedSym(v) => (*v).hash(state),
            FieldValue::Boxed(v) => v.hash_in_heap(heap, state),
            FieldValue::Callback => state.write_u8(42),
        }
    }
}

/// Iterates over fields in a record value
pub struct FieldValueIter<'cm> {
    pub(super) classmap_field_iter: class_map::FieldIterator<'cm>,
    pub(super) record_data: *const u8,
}

impl<'cm> Iterator for FieldValueIter<'cm> {
    type Item = FieldValue;

    #[allow(clippy::cast_ptr_alignment)]
    fn next(&mut self) -> Option<FieldValue> {
        use class_map::FieldType;

        self.classmap_field_iter
            .next()
            .map(|classmap_field| unsafe {
                let field_base_ptr = self.record_data.add(classmap_field.offset());

                match classmap_field.field_type() {
                    FieldType::Bool => FieldValue::Bool(*(field_base_ptr as *const bool)),
                    FieldType::Char => FieldValue::Char(*(field_base_ptr as *const char)),
                    FieldType::Float => FieldValue::Float(*(field_base_ptr as *const f64)),
                    FieldType::Int => FieldValue::Int(*(field_base_ptr as *const i64)),
                    FieldType::InternedSym => {
                        FieldValue::InternedSym(*(field_base_ptr as *const InternedSym))
                    }
                    FieldType::Boxed => {
                        FieldValue::Boxed(*(field_base_ptr as *const Gc<boxed::Any>))
                    }
                    FieldType::Callback => FieldValue::Callback,
                }
            })
    }
}

pub(crate) struct FieldGcRefIter<'cm> {
    pub(super) classmap_field_iter: class_map::FieldIterator<'cm>,
    pub(super) record_data: *const u8,
}

impl<'cm> FieldGcRefIter<'cm> {
    pub(crate) fn empty() -> FieldGcRefIter<'static> {
        FieldGcRefIter {
            classmap_field_iter: class_map::FieldIterator::empty(),
            record_data: ptr::null(),
        }
    }
}

impl<'cm> Iterator for FieldGcRefIter<'cm> {
    type Item = FieldGcRef;

    #[allow(clippy::cast_ptr_alignment)]
    fn next(&mut self) -> Option<FieldGcRef> {
        while let Some(classmap_field) = self.classmap_field_iter.next() {
            unsafe {
                use class_map::FieldType;
                let field_base_ptr = self.record_data.add(classmap_field.offset());

                match classmap_field.field_type() {
                    FieldType::InternedSym => {
                        return Some(FieldGcRef::InternedSym(
                            &mut *(field_base_ptr as *mut InternedSym),
                        ));
                    }
                    FieldType::Boxed => {
                        return Some(FieldGcRef::Boxed(
                            &mut *(field_base_ptr as *mut Gc<boxed::Any>),
                        ));
                    }
                    FieldType::Callback => {
                        use crate::callback::Callback;
                        use crate::task::Task;

                        let callback = &mut *(field_base_ptr
                            as *mut Callback<extern "C" fn(&mut Task, boxed::Closure)>);

                        return Some(FieldGcRef::Boxed(callback.closure_mut()));
                    }
                    _ => {}
                }
            }
        }

        None
    }
}
