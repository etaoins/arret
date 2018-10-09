use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::alloc::core::gen_alloced_box;
use crate::codegen::alloc::{ActiveAlloc, BoxSource};
use crate::codegen::CodegenCtx;

pub struct PairInput {
    pub llvm_head: LLVMValueRef,
    pub llvm_rest: LLVMValueRef,
    pub llvm_length: LLVMValueRef,
}

pub fn gen_alloc_int(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_source: BoxSource,
    llvm_int_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_int =
            gen_alloced_box::<boxed::Int>(cgx, builder, active_alloc, box_source, b"alloced_int\0");

        let value_ptr =
            LLVMBuildStructGEP(builder, alloced_int, 1, b"value_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, llvm_int_value, value_ptr);

        alloced_int
    }
}

pub fn gen_alloc_boxed_pair(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_source: BoxSource,
    input: &PairInput,
) -> LLVMValueRef {
    let PairInput {
        llvm_head,
        llvm_rest,
        llvm_length,
    } = input;

    unsafe {
        let alloced_pair = gen_alloced_box::<boxed::TopPair>(
            cgx,
            builder,
            active_alloc,
            box_source,
            b"alloced_pair\0",
        );

        let length_ptr = LLVMBuildStructGEP(
            builder,
            alloced_pair,
            1,
            b"length_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, *llvm_length, length_ptr);

        let head_ptr =
            LLVMBuildStructGEP(builder, alloced_pair, 2, b"head_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, *llvm_head, head_ptr);

        let rest_ptr =
            LLVMBuildStructGEP(builder, alloced_pair, 3, b"rest_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, *llvm_rest, rest_ptr);

        alloced_pair
    }
}
