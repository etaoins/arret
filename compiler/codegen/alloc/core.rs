use std::{mem, ptr};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMAttributeFunctionIndex, LLVMAttributeReturnIndex, LLVMIntPredicate};

use arret_runtime::boxed;

use crate::codegen::alloc::{ActiveAlloc, AllocAtom, BoxSource};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;

fn init_alloced_box_header(
    tcx: &mut TargetCtx,
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
        LLVMBuildStore(builder, tcx.llvm_box_header(header), header_ptr);
    }
}

fn gen_stack_alloced_box<T: boxed::ConstTagged>(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    llvm_type: LLVMTypeRef,
    value_name: &[u8],
) -> LLVMValueRef {
    unsafe {
        let type_tag = T::TYPE_TAG;

        let alloced_box = LLVMBuildAlloca(builder, llvm_type, value_name.as_ptr() as *const _);
        LLVMSetAlignment(alloced_box, mem::align_of::<T>() as u32);

        init_alloced_box_header(
            tcx,
            builder,
            alloced_box,
            boxed::Header::new(type_tag, boxed::AllocType::Stack),
        );

        alloced_box
    }
}

fn gen_heap_alloced_box<T: boxed::ConstTagged>(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_size: boxed::BoxSize,
    llvm_type: LLVMTypeRef,
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
                LLVMInt32TypeInContext(tcx.llx),
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
            LLVMPointerType(llvm_type, 0),
            value_name.as_ptr() as *const _,
        );

        init_alloced_box_header(
            tcx,
            builder,
            alloced_box,
            boxed::Header::new(type_tag, box_size.to_heap_alloc_type()),
        );

        alloced_box
    }
}

pub fn gen_alloced_box_with_llvm_type<T: boxed::ConstTagged>(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    llvm_type: LLVMTypeRef,
    value_name: &[u8],
) -> LLVMValueRef {
    match box_source {
        BoxSource::Stack => gen_stack_alloced_box::<T>(tcx, builder, llvm_type, value_name),
        BoxSource::Heap(box_size) => {
            gen_heap_alloced_box::<T>(tcx, builder, active_alloc, box_size, llvm_type, value_name)
        }
    }
}

pub fn gen_alloced_box<T: boxed::ConstTagged>(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    value_name: &[u8],
) -> LLVMValueRef {
    let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&T::TYPE_TAG.into());

    gen_alloced_box_with_llvm_type::<T>(
        tcx,
        builder,
        active_alloc,
        box_source,
        llvm_type,
        value_name,
    )
}

/// Allocates cells by invoking a function at runtime
///
/// This is the slow path; it is only used when our current heap segment is full.
fn gen_runtime_heap_alloc(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    builder: LLVMBuilderRef,
    llvm_task: LLVMValueRef,
    required_cells: usize,
) -> LLVMValueRef {
    use arret_runtime::abitype;

    unsafe {
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_param_types = &mut [tcx.task_llvm_ptr_type(), llvm_i32];

        let alloc_cells_llvm_type = LLVMFunctionType(
            tcx.boxed_abi_to_llvm_ptr_type(&abitype::BoxedABIType::Any),
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
                    tcx.llvm_enum_attr_for_name("cold", 0),
                );
                LLVMAddAttributeAtIndex(
                    alloc_cells_fun,
                    LLVMAttributeReturnIndex,
                    tcx.llvm_boxed_align_attr(),
                );
                LLVMAddAttributeAtIndex(
                    alloc_cells_fun,
                    LLVMAttributeReturnIndex,
                    tcx.llvm_noalias_attr(),
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
        let dereferenceable_attr = tcx.llvm_enum_attr_for_name(
            "dereferenceable",
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
pub fn atom_into_active_alloc<'op>(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    builder: LLVMBuilderRef,
    llvm_task: LLVMValueRef,
    atom: AllocAtom<'op>,
) -> ActiveAlloc<'op> {
    use arret_runtime::abitype;

    let required_cells = atom
        .box_sources
        .iter()
        .map(|box_source| match box_source {
            BoxSource::Stack => 0,
            BoxSource::Heap(box_size) => box_size.cell_count(),
        })
        .sum();

    if required_cells == 0 {
        return ActiveAlloc {
            box_slots: ptr::null_mut(),
            total_cells: 0,
            used_cells: 0,

            box_source_iter: atom.box_sources.into_iter(),
            cond_plan_iter: atom.cond_plans.into_iter(),
        };
    }

    unsafe {
        let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));

        let mut bump_alloc_block =
            LLVMAppendBasicBlockInContext(tcx.llx, function, b"bump_alloc\0".as_ptr() as *const _);

        let mut runtime_alloc_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            function,
            b"runtime_alloc\0".as_ptr() as *const _,
        );

        let cont_block =
            LLVMAppendBasicBlockInContext(tcx.llx, function, b"alloc_cont\0".as_ptr() as *const _);

        let seg_next_ptr = LLVMBuildStructGEP(
            builder,
            llvm_task,
            0,
            b"seg_next_ptr\0".as_ptr() as *const _,
        );
        let mut seg_old_next =
            LLVMBuildLoad(builder, seg_next_ptr, "seg_old_next\0".as_ptr() as *const _);

        let gep_indices = &mut [LLVMConstInt(
            LLVMInt32TypeInContext(tcx.llx),
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

        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);
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
            gen_runtime_heap_alloc(tcx, mcx, builder, llvm_task, required_cells);
        LLVMBuildBr(builder, cont_block);

        LLVMPositionBuilderAtEnd(builder, cont_block);
        let box_slots = LLVMBuildPhi(
            builder,
            tcx.boxed_abi_to_llvm_ptr_type(&abitype::BoxedABIType::Any),
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

            box_source_iter: atom.box_sources.into_iter(),
            cond_plan_iter: atom.cond_plans.into_iter(),
        }
    }
}
