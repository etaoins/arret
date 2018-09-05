use llvm_sys;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::codegen::CodegenCtx;

use runtime::abitype::{ABIType, BoxedABIType};

pub(super) fn convert_to_boxed_any(
    cgx: &mut CodegenCtx,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    from_abi_type: &ABIType,
    value: LLVMValueRef,
) -> LLVMValueRef {
    use runtime::boxed::TypeTag;

    match from_abi_type {
        ABIType::Boxed(BoxedABIType::Any) => value,
        ABIType::Bool => unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);
            let llvm_any_ptr = cgx.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);

            let one_value = LLVMConstInt(llvm_i8, 1, 0);
            let cond_pred = LLVMBuildICmp(
                builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                value,
                one_value,
                b"is_true\0".as_ptr() as *const _,
            );

            let true_ptr = LLVMConstBitCast(
                cgx.ptr_to_singleton_box(
                    module,
                    &BoxedABIType::DirectTagged(TypeTag::True),
                    b"ARRET_TRUE\0",
                ),
                llvm_any_ptr,
            );

            let false_ptr = LLVMConstBitCast(
                cgx.ptr_to_singleton_box(
                    module,
                    &BoxedABIType::DirectTagged(TypeTag::False),
                    b"ARRET_FALSE\0",
                ),
                llvm_any_ptr,
            );

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
