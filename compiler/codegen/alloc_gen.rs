use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::CodegenCtx;

fn gen_stack_alloced_box<T: boxed::DirectTagged>(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    value_name: &[u8],
) -> LLVMValueRef {
    unsafe {
        let type_tag = T::TYPE_TAG;
        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let alloced_box = LLVMBuildAlloca(builder, llvm_type, value_name.as_ptr() as *const _);
        LLVMSetAlignment(alloced_box, mem::align_of::<T>() as u32);

        let header_ptr = LLVMBuildStructGEP(
            builder,
            alloced_box,
            0,
            b"header_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(
            builder,
            cgx.llvm_box_header(boxed::Header::new(type_tag, boxed::AllocType::Stack)),
            header_ptr,
        );

        alloced_box
    }
}

pub fn gen_alloc_int(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    llvm_int_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_int = gen_stack_alloced_box::<boxed::Int>(cgx, builder, b"alloced_int\0");

        let value_ptr =
            LLVMBuildStructGEP(builder, alloced_int, 1, b"value_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, llvm_int_value, value_ptr);

        alloced_int
    }
}
