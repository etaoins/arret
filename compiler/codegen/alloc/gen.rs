use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::alloc::{ActiveAlloc, BoxSource};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;

pub struct PairInput {
    pub llvm_head: LLVMValueRef,
    pub llvm_rest: LLVMValueRef,
    pub llvm_length: LLVMValueRef,
}

fn init_alloced_box_header(
    cgx: &mut CodegenCtx,
    builder: LLVMBuilderRef,
    alloced_box: LLVMValueRef,
    header: boxed::Header,
) {
    unsafe {
        let header_ptr = LLVMBuildStructGEP(
            builder,
            alloced_box,
            0,
            b"header_ptr\0".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, cgx.llvm_box_header(header), header_ptr);
    }
}

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

        init_alloced_box_header(
            cgx,
            builder,
            alloced_box,
            boxed::Header::new(type_tag, boxed::AllocType::Stack),
        );

        alloced_box
    }
}

fn gen_heap_alloced_box<T: boxed::DirectTagged>(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_size: boxed::BoxSize,
    value_name: &[u8],
) -> LLVMValueRef {
    use runtime::abitype;
    unsafe {
        let llvm_i32 = LLVMInt32TypeInContext(cgx.llx);
        let llvm_param_types = &mut [cgx.task_llvm_type(), llvm_i32];

        let alloc_cells_llvm_type = LLVMFunctionType(
            cgx.boxed_abi_to_llvm_ptr_type(&abitype::BoxedABIType::Any),
            llvm_param_types.as_mut_ptr(),
            llvm_param_types.len() as u32,
            0,
        );

        let alloc_cells_fun = mcx.get_function_or_insert(
            alloc_cells_llvm_type,
            b"arret_runtime_alloc_cells\0",
            |alloc_cells_fun| {
                cgx.add_boxed_return_attrs(alloc_cells_fun);
            },
        );

        let alloc_cells_args = &mut [
            active_alloc.llvm_task,
            LLVMConstInt(llvm_i32, box_size.cell_count() as u64, 0),
        ];

        let cells_ret = LLVMBuildCall(
            builder,
            alloc_cells_fun,
            alloc_cells_args.as_mut_ptr(),
            alloc_cells_args.len() as u32,
            b"cell_alloc\0".as_ptr() as *const _,
        );

        let type_tag = T::TYPE_TAG;
        let alloced_box = LLVMBuildBitCast(
            builder,
            cells_ret,
            cgx.boxed_abi_to_llvm_ptr_type(&type_tag.into()),
            value_name.as_ptr() as *const _,
        );

        init_alloced_box_header(
            cgx,
            builder,
            alloced_box,
            boxed::Header::new(type_tag, box_size.to_heap_alloc_type()),
        );

        alloced_box
    }
}

fn gen_alloced_box<T: boxed::DirectTagged>(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_source: BoxSource,
    value_name: &[u8],
) -> LLVMValueRef {
    match box_source {
        BoxSource::Stack => gen_stack_alloced_box::<T>(cgx, builder, value_name),
        BoxSource::Heap(box_size) => {
            gen_heap_alloced_box::<T>(cgx, mcx, builder, active_alloc, box_size, value_name)
        }
    }
}

pub fn gen_alloc_int(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_source: BoxSource,
    llvm_int_value: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_int = gen_alloced_box::<boxed::Int>(
            cgx,
            mcx,
            builder,
            active_alloc,
            box_source,
            b"alloced_int\0",
        );

        let value_ptr =
            LLVMBuildStructGEP(builder, alloced_int, 1, b"value_ptr\0".as_ptr() as *const _);
        LLVMBuildStore(builder, llvm_int_value, value_ptr);

        alloced_int
    }
}

pub fn gen_alloc_boxed_pair(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
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
            mcx,
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
