use llvm_sys;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::codegen::CodegenCtx;

use runtime::abitype;
use runtime::boxed;

pub(super) fn ptr_to_singleton<T: boxed::Boxed>(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    singleton: &T,
    name: &[u8],
) -> LLVMValueRef {
    unsafe {
        let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);
        let llvm_any = cgx.boxed_abi_to_llvm_type(&abitype::BoxedABIType::Any);

        let ptr_int = LLVMConstInt(llvm_i64, singleton as *const _ as u64, 0);
        LLVMBuildIntToPtr(builder, ptr_int, llvm_any, name.as_ptr() as *const _)
    }
}

pub(super) fn convert_to_boxed_any(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    from_abi_type: &abitype::ABIType,
    value: LLVMValueRef,
) -> LLVMValueRef {
    match from_abi_type {
        abitype::ABIType::Boxed(abitype::BoxedABIType::Any) => value,
        abitype::ABIType::Bool => unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

            let one_value = LLVMConstInt(llvm_i8, 1, 0);
            let cond_pred = LLVMBuildICmp(
                builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                value,
                one_value,
                b"is_true\0".as_ptr() as *const _,
            );

            let true_ptr = ptr_to_singleton(cgx, builder, &boxed::TRUE_INSTANCE, b"true_ptr\0");
            let false_ptr = ptr_to_singleton(cgx, builder, &boxed::FALSE_INSTANCE, b"false_ptr\0");

            LLVMBuildSelect(
                builder,
                cond_pred,
                true_ptr,
                false_ptr,
                b"bool_ptr\0".as_ptr() as *const _,
            )
        },
        other => {
            unimplemented!("Type conversion from {:?}", other);
        }
    }
}
