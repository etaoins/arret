use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::alloc::core::gen_alloced_box;
use crate::codegen::alloc::{ActiveAlloc, BoxSource};
use crate::codegen::target_gen::TargetCtx;

pub struct PairInput {
    pub llvm_head: LLVMValueRef,
    pub llvm_rest: LLVMValueRef,
    pub llvm_length: LLVMValueRef,
}

pub struct FunThunkInput {
    pub llvm_closure: LLVMValueRef,
    pub llvm_entry_point: LLVMValueRef,
}

pub fn gen_alloc_int(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    llvm_int_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_int =
            gen_alloced_box::<boxed::Int>(tcx, builder, active_alloc, box_source, b"alloced_int\0");

        let value_ptr =
            LLVMBuildStructGEP(builder, alloced_int, 1, b"value_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, llvm_int_value, value_ptr);

        alloced_int
    }
}

pub fn gen_alloc_char(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    llvm_char_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_char = gen_alloced_box::<boxed::Char>(
            tcx,
            builder,
            active_alloc,
            box_source,
            b"alloced_char\0",
        );

        let value_ptr = LLVMBuildStructGEP(
            builder,
            alloced_char,
            1,
            b"value_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, llvm_char_value, value_ptr);

        alloced_char
    }
}

pub fn gen_alloc_float(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    llvm_float_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_float = gen_alloced_box::<boxed::Float>(
            tcx,
            builder,
            active_alloc,
            box_source,
            b"alloced_float\0",
        );

        let value_ptr = LLVMBuildStructGEP(
            builder,
            alloced_float,
            1,
            b"value_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, llvm_float_value, value_ptr);

        alloced_float
    }
}

pub fn gen_alloc_boxed_pair(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    input: &PairInput,
) -> LLVMValueRef {
    let PairInput {
        llvm_head,
        llvm_rest,
        llvm_length,
    } = input;

    unsafe {
        let alloced_pair = gen_alloced_box::<boxed::Pair>(
            tcx,
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

pub fn gen_alloc_boxed_fun_thunk(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    input: &FunThunkInput,
) -> LLVMValueRef {
    let FunThunkInput {
        llvm_closure,
        llvm_entry_point,
    } = input;

    unsafe {
        let alloced_fun_thunk = gen_alloced_box::<boxed::FunThunk>(
            tcx,
            builder,
            active_alloc,
            box_source,
            b"alloced_fun_thunk\0",
        );

        let closure_ptr = LLVMBuildStructGEP(
            builder,
            alloced_fun_thunk,
            1,
            b"closure_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, *llvm_closure, closure_ptr);

        let entry_point_ptr = LLVMBuildStructGEP(
            builder,
            alloced_fun_thunk,
            2,
            b"entry_point_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, *llvm_entry_point, entry_point_ptr);

        alloced_fun_thunk
    }
}
