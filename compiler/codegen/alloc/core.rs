use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMAttributeFunctionIndex, LLVMAttributeReturnIndex, LLVMIntPredicate};

use runtime::boxed;

use crate::codegen::alloc::{ActiveAlloc, AllocAtom, BoxSource};
use crate::codegen::context::CodegenCtx;
use crate::codegen::mod_gen::ModCtx;

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

pub fn gen_alloced_box<T: boxed::DirectTagged>(
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

/// Allocates cells by invoking a function at runtime
///
/// This is the slow path; it is only used when our current heap segment is full.
fn gen_runtime_heap_alloc(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    builder: LLVMBuilderRef,
    llvm_task: LLVMValueRef,
    required_cells: usize,
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
                LLVMAddAttributeAtIndex(
                    alloc_cells_fun,
                    LLVMAttributeFunctionIndex,
                    cgx.llvm_enum_attr_for_name(b"cold", 0),
                );
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

        let runtime_box_slots = LLVMBuildCall(
            builder,
            alloc_cells_fun,
            alloc_cells_args.as_mut_ptr(),
            alloc_cells_args.len() as u32,
            b"runtime_box_slots\0".as_ptr() as *const _,
        );

        // We can dereference the entire allocation immediately
        let dereferenceable_attr = cgx.llvm_enum_attr_for_name(
            b"dereferenceable",
            (mem::size_of::<boxed::Any>() * required_cells) as u64,
        );
        LLVMAddCallSiteAttribute(
            runtime_box_slots,
            LLVMAttributeReturnIndex,
            dereferenceable_attr,
        );

        runtime_box_slots
    }
}

/// Generates an `ActiveAlloc` containing the required allocations for the passed `AllocAtom`
///
/// This will first attempt a bump allocation on the task's current segment. If that fails it will
/// fallback to the runtime.
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
        let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));

        let mut bump_alloc_block =
            LLVMAppendBasicBlockInContext(cgx.llx, function, b"bump_alloc\0".as_ptr() as *const _);

        let mut runtime_alloc_block = LLVMAppendBasicBlockInContext(
            cgx.llx,
            function,
            b"runtime_alloc\0".as_ptr() as *const _,
        );

        let cont_block =
            LLVMAppendBasicBlockInContext(cgx.llx, function, b"alloc_cont\0".as_ptr() as *const _);

        let seg_next_ptr = LLVMBuildStructGEP(
            builder,
            llvm_task,
            0,
            b"seg_next_ptr\0".as_ptr() as *const _,
        );
        let mut seg_old_next =
            LLVMBuildLoad(builder, seg_next_ptr, "seg_old_next\0".as_ptr() as *const _);

        let gep_indices = &mut [LLVMConstInt(
            LLVMInt32TypeInContext(cgx.llx),
            required_cells as u64,
            0,
        )];
        let seg_new_next = LLVMBuildInBoundsGEP(
            builder,
            seg_old_next,
            gep_indices.as_mut_ptr(),
            gep_indices.len() as u32,
            b"seg_new_next\0".as_ptr() as *const _,
        );

        let seg_end_ptr =
            LLVMBuildStructGEP(builder, llvm_task, 1, b"seg_end_ptr\0".as_ptr() as *const _);
        let seg_end = LLVMBuildLoad(builder, seg_end_ptr, "seg_end\0".as_ptr() as *const _);

        let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);
        let seg_new_next_int = LLVMBuildPtrToInt(
            builder,
            seg_new_next,
            llvm_i64,
            "seg_new_next_int\0".as_ptr() as *const _,
        );
        let seg_end_int = LLVMBuildPtrToInt(
            builder,
            seg_end,
            llvm_i64,
            "seg_end_int\0".as_ptr() as *const _,
        );

        let seg_has_space = LLVMBuildICmp(
            builder,
            LLVMIntPredicate::LLVMIntULE,
            seg_new_next_int,
            seg_end_int,
            "seg_has_space\0".as_ptr() as *const _,
        );

        LLVMBuildCondBr(
            builder,
            seg_has_space,
            bump_alloc_block,
            runtime_alloc_block,
        );

        // Bump alloc succeeded; update the segment
        LLVMPositionBuilderAtEnd(builder, bump_alloc_block);
        LLVMBuildStore(builder, seg_new_next, seg_next_ptr);
        LLVMBuildBr(builder, cont_block);

        // Bump alloc failed; call the runtime
        LLVMPositionBuilderAtEnd(builder, runtime_alloc_block);
        let mut runtime_box_slots =
            gen_runtime_heap_alloc(cgx, mcx, builder, llvm_task, required_cells);
        LLVMBuildBr(builder, cont_block);

        LLVMPositionBuilderAtEnd(builder, cont_block);
        let box_slots = LLVMBuildPhi(
            builder,
            cgx.boxed_abi_to_llvm_ptr_type(&abitype::BoxedABIType::Any),
            b"box_slots\0".as_ptr() as *const _,
        );

        LLVMAddIncoming(
            box_slots,
            &mut seg_old_next as *mut _,
            &mut bump_alloc_block as *mut _,
            1,
        );
        LLVMAddIncoming(
            box_slots,
            &mut runtime_box_slots as *mut _,
            &mut runtime_alloc_block as *mut _,
            1,
        );

        ActiveAlloc {
            box_slots,
            total_cells: required_cells,
            used_cells: 0,
        }
    }
}
