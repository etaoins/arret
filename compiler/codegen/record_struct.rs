use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;

use arret_runtime::boxed;
use arret_runtime::class_map;

use crate::codegen::const_gen::annotate_private_global;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;

pub const IS_INLINE_INDEX: u32 = 1;
pub const CONTAINS_GC_REFS_INDEX: u32 = 2;
pub const RECORD_CLASS_ID_INDEX: u32 = 3;
pub const INLINE_DATA_INDEX: u32 = 4;

/// Adds internal member fields common to all inline and large records
pub fn append_common_internal_members(tcx: &mut TargetCtx, members: &mut Vec<LLVMTypeRef>) {
    unsafe {
        members.extend_from_slice(&[
            // is_inline
            LLVMInt8TypeInContext(tcx.llx),
            // contains_gc_refs
            LLVMInt8TypeInContext(tcx.llx),
            // record_class_id
            tcx.record_class_id_llvm_type(),
        ]);
    }
}

#[derive(Clone)]
pub struct TargetRecordStruct {
    pub data_len: usize,
    pub record_storage: boxed::RecordStorage,
    pub llvm_data_type: LLVMTypeRef,
    pub classmap_class: class_map::BoxedClass,
}

impl TargetRecordStruct {
    pub fn from_mir_record_struct(
        tcx: &mut TargetCtx,
        record_struct: &ops::RecordStructId,
    ) -> Self {
        let mut members: Box<[LLVMTypeRef]> = record_struct
            .field_abi_types
            .iter()
            .map(|abi_type| tcx.abi_to_llvm_type(abi_type))
            .collect();

        let record_data_name =
            ffi::CString::new(format!("{}_data", record_struct.source_name)).unwrap();

        let llvm_data_type = unsafe {
            let llvm_data_type = LLVMStructCreateNamed(
                tcx.llx,
                record_data_name.as_bytes_with_nul().as_ptr() as *const _,
            );

            LLVMStructSetBody(
                llvm_data_type,
                members.as_mut_ptr(),
                members.len() as u32,
                0,
            );

            // Our record data is only 8 byte aligned
            assert!(LLVMABIAlignmentOfType(tcx.target_data(), llvm_data_type) <= 8);

            llvm_data_type
        };

        let data_len =
            unsafe { ((LLVMSizeOfTypeInBits(tcx.target_data(), llvm_data_type) + 7) / 8) as usize };

        let record_storage = boxed::Record::storage_for_data_len(data_len);

        let classmap_class = class_map::BoxedClass::from_fields(
            record_struct
                .field_abi_types
                .iter()
                .enumerate()
                .map(|(index, field_abi_type)| {
                    let field_type = class_map::FieldType::from_abi_type(field_abi_type);
                    let offset = unsafe {
                        LLVMOffsetOfElement(tcx.target_data(), llvm_data_type, index as u32)
                            as usize
                    };

                    class_map::Field::new(field_type, offset)
                }),
        );

        Self {
            data_len,
            record_storage,
            llvm_data_type,
            classmap_class,
        }
    }
}

pub fn gen_classmap_classes<'a>(
    tcx: &'a mut TargetCtx,
    llvm_module: LLVMModuleRef,
    record_structs: &[ops::RecordStructId],
) -> LLVMValueRef {
    if record_structs.is_empty() {
        return unsafe { LLVMConstPointerNull(LLVMPointerType(tcx.classmap_class_type(), 0)) };
    }

    let llvm_classmap_field_type = tcx.classmap_field_type();
    let llvm_i8 = unsafe { LLVMInt8TypeInContext(tcx.llx) };
    let llvm_i32 = unsafe { LLVMInt32TypeInContext(tcx.llx) };

    let llvm_first_element_gep_indices =
        unsafe { &mut [LLVMConstInt(llvm_i32, 0, 0), LLVMConstInt(llvm_i32, 0, 0)] };

    let mut llvm_classmap_classes: Vec<LLVMValueRef> = record_structs
        .iter()
        .map(|record_struct| {
            let classmap_class = tcx
                .target_record_struct(record_struct)
                .classmap_class
                .as_ref();

            if classmap_class.is_empty() {
                return unsafe {
                    LLVMConstPointerNull(LLVMPointerType(llvm_classmap_field_type, 0))
                };
            }

            let mut llvm_classmap_class_fields: Vec<LLVMValueRef> = classmap_class
                .field_iter()
                .map(|field| unsafe {
                    // This is the layout of `class_map::Field`
                    let llvm_offset = LLVMConstInt(llvm_i32, field.offset() as u64, 0);
                    let llvm_field_type = LLVMConstInt(llvm_i8, field.field_type() as u64, 0);
                    let llvm_is_last = LLVMConstInt(llvm_i8, field.is_last() as u64, 0);

                    let members = &mut [llvm_offset, llvm_field_type, llvm_is_last];

                    LLVMConstNamedStruct(
                        llvm_classmap_field_type,
                        members.as_mut_ptr(),
                        members.len() as u32,
                    )
                })
                .collect();

            unsafe {
                let llvm_classmap_class = LLVMConstArray(
                    llvm_classmap_field_type,
                    llvm_classmap_class_fields.as_mut_ptr(),
                    llvm_classmap_class_fields.len() as u32,
                );

                let classmap_class_global_name =
                    ffi::CString::new(format!("{}_classmap", record_struct.source_name)).unwrap();

                let llvm_classmap_class_global = LLVMAddGlobal(
                    llvm_module,
                    LLVMTypeOf(llvm_classmap_class),
                    classmap_class_global_name.as_bytes_with_nul().as_ptr() as *const _,
                );

                LLVMSetInitializer(llvm_classmap_class_global, llvm_classmap_class);
                annotate_private_global(llvm_classmap_class_global);

                LLVMConstInBoundsGEP(
                    llvm_classmap_class_global,
                    llvm_first_element_gep_indices.as_mut_ptr(),
                    llvm_first_element_gep_indices.len() as u32,
                )
            }
        })
        .collect();

    unsafe {
        let llvm_classmap = LLVMConstArray(
            LLVMPointerType(llvm_classmap_field_type, 0),
            llvm_classmap_classes.as_mut_ptr(),
            llvm_classmap_classes.len() as u32,
        );

        let llvm_classmap_global = LLVMAddGlobal(
            llvm_module,
            LLVMTypeOf(llvm_classmap),
            "classmap_classes\0".as_ptr() as *const _,
        );

        LLVMSetInitializer(llvm_classmap_global, llvm_classmap);
        annotate_private_global(llvm_classmap_global);

        LLVMConstInBoundsGEP(
            llvm_classmap_global,
            llvm_first_element_gep_indices.as_mut_ptr(),
            llvm_first_element_gep_indices.len() as u32,
        )
    }
}
