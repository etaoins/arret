use std::{mem, ptr};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMAttributeReturnIndex;

use runtime::boxed;

use crate::codegen::alloc::{AllocAtom, BoxSource};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;

pub struct ActiveAlloc {
    box_slots: LLVMValueRef,

    total_cells: usize,
    used_cells: usize,
}

impl ActiveAlloc {
    pub fn empty() -> ActiveAlloc {
        ActiveAlloc {
            box_slots: ptr::null_mut(),
            total_cells: 0,
            used_cells: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.total_cells == self.used_cells
    }
}

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
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_size: boxed::BoxSize,
    value_name: &[u8],
) -> LLVMValueRef {
    unsafe {
        assert!(
            !active_alloc.is_empty(),
            "attempt to create heap box with empty active heap allocation"
        );

        let cell_count = box_size.cell_count();

        let slot_index = active_alloc.used_cells;
        let llvm_slot = if slot_index == 0 {
            active_alloc.box_slots
        } else {
            let gep_indices = &mut [LLVMConstInt(
                LLVMInt32TypeInContext(cgx.llx),
                slot_index as u64,
                0,
            )];

            LLVMBuildInBoundsGEP(
                builder,
                active_alloc.box_slots,
                gep_indices.as_mut_ptr(),
                gep_indices.len() as u32,
                b"slot\0".as_ptr() as *const _,
            )
        };

        active_alloc.used_cells += cell_count;
        assert!(active_alloc.used_cells <= active_alloc.total_cells);

        let type_tag = T::TYPE_TAG;
        let alloced_box = LLVMBuildBitCast(
            builder,
            llvm_slot,
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
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc,
    box_source: BoxSource,
    value_name: &[u8],
) -> LLVMValueRef {
    match box_source {
        BoxSource::Stack => gen_stack_alloced_box::<T>(cgx, builder, value_name),
        BoxSource::Heap(box_size) => {
            gen_heap_alloced_box::<T>(cgx, builder, active_alloc, box_size, value_name)
        }
    }
}

pub fn gen_active_alloc_for_atom(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    builder: LLVMBuilderRef,
    llvm_task: LLVMValueRef,
    atom: &AllocAtom<'_>,
) -> ActiveAlloc {
    use runtime::abitype;

    let required_cells = atom
        .box_sources
        .values()
        .map(|box_source| match box_source {
            BoxSource::Stack => 0,
            BoxSource::Heap(box_size) => box_size.cell_count(),
        })
        .sum();

    if required_cells == 0 {
        return ActiveAlloc::empty();
    }

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
                LLVMAddAttributeAtIndex(
                    alloc_cells_fun,
                    LLVMAttributeReturnIndex,
                    cgx.llvm_boxed_align_attr(),
                );
                LLVMAddAttributeAtIndex(
                    alloc_cells_fun,
                    LLVMAttributeReturnIndex,
                    cgx.llvm_noalias_attr(),
                );
            },
        );

        let alloc_cells_args = &mut [llvm_task, LLVMConstInt(llvm_i32, required_cells as u64, 0)];

        let box_slots = LLVMBuildCall(
            builder,
            alloc_cells_fun,
            alloc_cells_args.as_mut_ptr(),
            alloc_cells_args.len() as u32,
            b"box_slots\0".as_ptr() as *const _,
        );

        // We can dereference the entire allocation immediately
        let dereferenceable_attr = cgx.llvm_enum_attr_for_name(
            b"dereferenceable",
            (mem::size_of::<boxed::Any>() * required_cells) as u64,
        );
        LLVMAddCallSiteAttribute(box_slots, LLVMAttributeReturnIndex, dereferenceable_attr);

        ActiveAlloc {
            box_slots,
            total_cells: required_cells,
            used_cells: 0,
        }
    }
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
