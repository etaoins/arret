use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;

use arret_runtime::boxed;

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

#[derive(Clone, Copy)]
pub struct TargetRecordStruct {
    pub data_len: usize,
    pub record_storage: boxed::RecordStorage,
    pub llvm_data_type: LLVMTypeRef,
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

        let record_storage =
            boxed::Record::storage_for_data_len(data_len, tcx.pointer_bits() as usize);

        Self {
            data_len,
            record_storage,
            llvm_data_type,
        }
    }
}
