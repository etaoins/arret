use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::codegen::target_gen::TargetCtx;

pub const IS_INLINE_INDEX: u32 = 1;
pub const CONTAINS_GC_REFS_INDEX: u32 = 2;
pub const RECORD_CLASS_ID_INDEX: u32 = 3;
pub const INLINE_DATA_INDEX: u32 = 4;

pub fn append_common_members(tcx: &mut TargetCtx, members: &mut Vec<LLVMTypeRef>) {
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
