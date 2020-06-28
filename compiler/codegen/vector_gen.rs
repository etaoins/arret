use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate;

use arret_runtime::boxed;

use crate::codegen::fun_gen::FunCtx;
use crate::codegen::target_gen::TargetCtx;

fn load_boxed_external_vector_len(
    tcx: &mut TargetCtx,
    fcx: &mut FunCtx,
    llvm_boxed_vector: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let boxed_external_vector_ptr_type =
            LLVMPointerType(tcx.boxed_external_vector_llvm_type(), 0);

        let llvm_boxed_external_vector = LLVMBuildBitCast(
            fcx.builder,
            llvm_boxed_vector,
            boxed_external_vector_ptr_type,
            b"boxed_external_vector\0".as_ptr() as *const _,
        );

        let vector_external_len_ptr = LLVMBuildStructGEP(
            fcx.builder,
            llvm_boxed_external_vector,
            2,
            b"vector_external_len_ptr\0".as_ptr() as *const _,
        );

        let llvm_vector_external_len = LLVMBuildLoad(
            fcx.builder,
            vector_external_len_ptr,
            "vector_external_len\0".as_ptr() as *const _,
        );
        tcx.add_invariant_load_metadata(llvm_vector_external_len);

        llvm_vector_external_len
    }
}

pub(crate) fn load_boxed_vector_len(
    tcx: &mut TargetCtx,
    fcx: &mut FunCtx,
    llvm_boxed_vector: LLVMValueRef,
) -> LLVMValueRef {
    use arret_runtime::boxed::Vector;

    unsafe {
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let vector_inline_len_ptr = LLVMBuildStructGEP(
            fcx.builder,
            llvm_boxed_vector,
            1,
            b"vector_inline_len_ptr\0".as_ptr() as *const _,
        );

        let llvm_vector_inline_len = LLVMBuildLoad(
            fcx.builder,
            vector_inline_len_ptr,
            "vector_inline_len\0".as_ptr() as *const _,
        );
        tcx.add_invariant_load_metadata(llvm_vector_inline_len);

        let mut llvm_range_values = [
            LLVMValueAsMetadata(LLVMConstInt(llvm_i32, 0, 0)),
            LLVMValueAsMetadata(LLVMConstInt(
                llvm_i32,
                (Vector::<boxed::Any>::EXTERNAL_INLINE_LEN + 1) as u64,
                0,
            )),
        ];

        let range_md_kind_id = tcx.llvm_md_kind_id_for_name(b"vector_inline_len_range");
        let vector_inline_len_range_md = LLVMMDNodeInContext2(
            tcx.llx,
            llvm_range_values.as_mut_ptr(),
            llvm_range_values.len(),
        );
        LLVMSetMetadata(
            llvm_vector_inline_len,
            range_md_kind_id,
            LLVMMetadataAsValue(tcx.llx, vector_inline_len_range_md),
        );

        let llvm_vector_is_external = LLVMBuildICmp(
            fcx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            llvm_vector_inline_len,
            LLVMConstInt(
                llvm_i32,
                Vector::<boxed::Any>::EXTERNAL_INLINE_LEN as u64,
                0,
            ),
            "vector_is_external\0".as_ptr() as *const _,
        );

        let mut external_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"external_vector\0".as_ptr() as *const _,
        );
        let mut inline_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"inline_vector\0".as_ptr() as *const _,
        );
        let cont_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"vector_len_cont\0".as_ptr() as *const _,
        );

        LLVMBuildCondBr(
            fcx.builder,
            llvm_vector_is_external,
            external_block,
            inline_block,
        );

        let mut llvm_external_vector_len = {
            LLVMPositionBuilderAtEnd(fcx.builder, external_block);
            let llvm_value = load_boxed_external_vector_len(tcx, fcx, llvm_boxed_vector);

            LLVMBuildBr(fcx.builder, cont_block);
            llvm_value
        };

        let mut llvm_vector_inline_len_ext = {
            LLVMPositionBuilderAtEnd(fcx.builder, inline_block);
            let llvm_value = LLVMBuildZExt(
                fcx.builder,
                llvm_vector_inline_len,
                llvm_i64,
                b"vector_inline_len_ext\0".as_ptr() as *const _,
            );

            LLVMBuildBr(fcx.builder, cont_block);
            llvm_value
        };

        LLVMPositionBuilderAtEnd(fcx.builder, cont_block);
        let phi_value = LLVMBuildPhi(fcx.builder, llvm_i64, b"vector_len\0".as_ptr() as *const _);

        LLVMAddIncoming(
            phi_value,
            &mut llvm_external_vector_len as *mut _,
            &mut external_block as *mut _,
            1,
        );
        LLVMAddIncoming(
            phi_value,
            &mut llvm_vector_inline_len_ext as *mut _,
            &mut inline_block as *mut _,
            1,
        );

        phi_value
    }
}

pub(crate) fn load_boxed_vector_member(
    tcx: &mut TargetCtx,
    fcx: &mut FunCtx,
    llvm_boxed_vector: LLVMValueRef,
    known_vector_len: usize,
    member_index: usize,
) -> LLVMValueRef {
    if known_vector_len > boxed::Vector::<boxed::Any>::MAX_INLINE_LEN {
        // TODO: This will need to be done now that we implement persistent vectors
        todo!("loading members from external vectors")
    }

    unsafe {
        let boxed_inline_vector_ptr_type = LLVMPointerType(tcx.boxed_inline_vector_llvm_type(), 0);

        let llvm_boxed_inline_vector = LLVMBuildBitCast(
            fcx.builder,
            llvm_boxed_vector,
            boxed_inline_vector_ptr_type,
            b"boxed_inline_vector\0".as_ptr() as *const _,
        );

        let value_ptr = LLVMBuildStructGEP(
            fcx.builder,
            llvm_boxed_inline_vector,
            // Skip the header and inline len
            (2 + member_index) as u32,
            b"vector_member_ptr\0".as_ptr() as *const _,
        );

        let llvm_value = LLVMBuildLoad(
            fcx.builder,
            value_ptr,
            "vector_member_ptr\0".as_ptr() as *const _,
        );

        tcx.add_invariant_load_metadata(llvm_value);
        llvm_value
    }
}
