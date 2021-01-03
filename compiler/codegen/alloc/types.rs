use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMAttributeReturnIndex;

use arret_runtime::boxed;

use crate::codegen::alloc::core::{gen_alloced_box, gen_alloced_box_with_llvm_type};
use crate::codegen::alloc::{ActiveAlloc, BoxSource};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::libcstr;
use crate::mir::ops::RecordStructId;

pub struct PairInput {
    pub llvm_head: LLVMValueRef,
    pub llvm_rest: LLVMValueRef,
    pub llvm_list_len: LLVMValueRef,
}

pub struct FunThunkInput {
    pub llvm_captures: LLVMValueRef,
    pub llvm_entry_point: LLVMValueRef,
}

pub struct RecordInput<'rs> {
    pub record_struct: &'rs RecordStructId,
    pub llvm_fields: Box<[LLVMValueRef]>,
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

        let value_ptr = LLVMBuildStructGEP(builder, alloced_int, 1, libcstr!("value_ptr"));
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

        let value_ptr = LLVMBuildStructGEP(builder, alloced_char, 1, libcstr!("value_ptr"));
        LLVMBuildStore(builder, llvm_char_value, value_ptr);

        alloced_char
    }
}

pub fn gen_alloc_sym(
    tcx: &mut TargetCtx,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    llvm_interned_sym: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let alloced_sym =
            gen_alloced_box::<boxed::Sym>(tcx, builder, active_alloc, box_source, b"alloced_sym\0");

        let interned_sym_ptr =
            LLVMBuildStructGEP(builder, alloced_sym, 1, libcstr!("interned_sym_ptr"));
        LLVMBuildStore(builder, llvm_interned_sym, interned_sym_ptr);

        alloced_sym
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

        let value_ptr = LLVMBuildStructGEP(builder, alloced_float, 1, libcstr!("value_ptr"));
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
        llvm_list_len,
    } = input;

    unsafe {
        let alloced_pair = gen_alloced_box::<boxed::Pair>(
            tcx,
            builder,
            active_alloc,
            box_source,
            b"alloced_pair\0",
        );

        let list_len_ptr = LLVMBuildStructGEP(builder, alloced_pair, 1, libcstr!("list_len_ptr"));
        LLVMBuildStore(builder, *llvm_list_len, list_len_ptr);

        let head_ptr = LLVMBuildStructGEP(builder, alloced_pair, 2, libcstr!("head_ptr"));
        LLVMBuildStore(builder, *llvm_head, head_ptr);

        let rest_ptr = LLVMBuildStructGEP(builder, alloced_pair, 3, libcstr!("rest_ptr"));
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
        llvm_captures,
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

        let captures_ptr =
            LLVMBuildStructGEP(builder, alloced_fun_thunk, 1, libcstr!("captures_ptr"));
        LLVMBuildStore(builder, *llvm_captures, captures_ptr);

        let entry_point_ptr =
            LLVMBuildStructGEP(builder, alloced_fun_thunk, 2, libcstr!("entry_point_ptr"));
        LLVMBuildStore(builder, *llvm_entry_point, entry_point_ptr);

        alloced_fun_thunk
    }
}

pub fn gen_alloc_boxed_record(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    builder: LLVMBuilderRef,
    active_alloc: &mut ActiveAlloc<'_>,
    box_source: BoxSource,
    input: &RecordInput<'_>,
) -> LLVMValueRef {
    let RecordInput {
        record_struct,
        llvm_fields,
    } = input;

    let record_class_id = mcx.record_class_id_for_struct(record_struct);

    unsafe {
        let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);

        let record_struct::TargetRecordStruct {
            data_layout,
            record_storage,
            llvm_data_type,
            ..
        } = *tcx.target_record_struct(record_struct);

        let may_contain_gc_refs = record_struct
            .field_abi_types
            .iter()
            .zip(llvm_fields.iter())
            .any(|(field_abi_type, llvm_field)| {
                field_abi_type.may_contain_gc_refs() && LLVMIsConstant(*llvm_field) == 0
            });

        let boxed_record_name = format!("alloced_{}_record\0", record_struct.source_name);

        let llvm_box_type = tcx.record_struct_llvm_box_type(record_struct);
        let alloced_boxed_record = gen_alloced_box_with_llvm_type::<boxed::Record>(
            tcx,
            builder,
            active_alloc,
            box_source,
            llvm_box_type,
            boxed_record_name.as_bytes(),
        );

        let may_contain_gc_refs_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::CONTAINS_GC_REFS_INDEX,
            libcstr!("may_contain_gc_refs_ptr"),
        );
        let llvm_may_contain_gc_refs = LLVMConstInt(llvm_i8, may_contain_gc_refs as u64, 1);
        LLVMBuildStore(builder, llvm_may_contain_gc_refs, may_contain_gc_refs_ptr);

        let record_class_id_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::RECORD_CLASS_ID_INDEX,
            libcstr!("record_class_id_ptr"),
        );

        let llvm_record_class_id = LLVMConstInt(
            tcx.record_class_id_llvm_type(),
            u64::from(record_class_id),
            1,
        );
        LLVMBuildStore(builder, llvm_record_class_id, record_class_id_ptr);

        // This is used by both inline and external records
        let record_data_gep_indices = &mut [
            LLVMConstInt(llvm_i32, 0, 0),
            LLVMConstInt(llvm_i32, u64::from(record_struct::DATA_INDEX), 0),
        ];

        let (llvm_record_data_ptr, inline_byte_len) = match (record_storage, box_source) {
            (boxed::RecordStorage::Inline(_), _) => {
                let llvm_inline_record_data_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_data_gep_indices.as_mut_ptr(),
                    record_data_gep_indices.len() as u32,
                    libcstr!("inline_record_data"),
                );

                let inline_byte_len = match data_layout {
                    Some(data_layout) => data_layout.size(),
                    None => 0,
                };

                (llvm_inline_record_data_ptr, inline_byte_len)
            }
            (boxed::RecordStorage::External, BoxSource::Stack) => {
                // Allocate the record data
                let llvm_stack_record_data_ptr =
                    LLVMBuildAlloca(builder, llvm_data_type, libcstr!("stack_record_data"));

                // Update our record data pointer
                let llvm_record_data_ptr_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_data_gep_indices.as_mut_ptr(),
                    record_data_gep_indices.len() as u32,
                    libcstr!("record_data_ptr_ptr"),
                );

                LLVMBuildStore(
                    builder,
                    llvm_stack_record_data_ptr,
                    llvm_record_data_ptr_ptr,
                );

                (
                    llvm_stack_record_data_ptr,
                    boxed::Record::EXTERNAL_INLINE_LEN as usize,
                )
            }
            (boxed::RecordStorage::External, BoxSource::Heap(_)) => {
                let data_layout = data_layout.unwrap();

                let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
                let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
                let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

                let llvm_param_types = &mut [llvm_i64, llvm_i32];

                let alloc_record_data_llvm_type = LLVMFunctionType(
                    LLVMPointerType(llvm_i8, 0),
                    llvm_param_types.as_mut_ptr(),
                    llvm_param_types.len() as u32,
                    0,
                );

                let alloc_record_data_fun = mcx.get_function_or_insert(
                    alloc_record_data_llvm_type,
                    b"arret_runtime_alloc_record_data\0",
                    |alloc_record_data_fun| {
                        LLVMAddAttributeAtIndex(
                            alloc_record_data_fun,
                            LLVMAttributeReturnIndex,
                            tcx.llvm_noalias_attr(),
                        );
                    },
                );

                let alloc_record_data_args = &mut [
                    LLVMConstInt(llvm_i64, data_layout.size() as u64, 0),
                    LLVMConstInt(llvm_i32, data_layout.align() as u64, 0),
                ];

                let llvm_untyped_record_data_ptr = LLVMBuildCall(
                    builder,
                    alloc_record_data_fun,
                    alloc_record_data_args.as_mut_ptr(),
                    alloc_record_data_args.len() as u32,
                    libcstr!("external_record_data"),
                );

                // Convert the record data pointer to the correct type
                let llvm_typed_record_data_ptr = LLVMBuildBitCast(
                    builder,
                    llvm_untyped_record_data_ptr,
                    LLVMPointerType(llvm_data_type, 0),
                    libcstr!("typed_record_data_ptr"),
                );

                // Save the record data pointer
                let llvm_record_data_ptr_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_data_gep_indices.as_mut_ptr(),
                    record_data_gep_indices.len() as u32,
                    libcstr!("record_data_ptr_ptr"),
                );
                LLVMBuildStore(
                    builder,
                    llvm_typed_record_data_ptr,
                    llvm_record_data_ptr_ptr,
                );

                // Save the compact layout
                let record_compact_layout_gep_indices = &mut [
                    LLVMConstInt(llvm_i32, 0, 0),
                    LLVMConstInt(
                        llvm_i32,
                        u64::from(record_struct::EXTERNAL_COMPACT_LAYOUT_INDEX),
                        0,
                    ),
                ];

                let llvm_record_compact_layout_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_compact_layout_gep_indices.as_mut_ptr(),
                    record_compact_layout_gep_indices.len() as u32,
                    libcstr!("record_compact_layout_ptr"),
                );
                LLVMBuildStore(
                    builder,
                    LLVMConstInt(
                        llvm_i64,
                        boxed::RecordData::alloc_layout_to_compact(Some(data_layout)),
                        0,
                    ),
                    llvm_record_compact_layout_ptr,
                );

                (
                    llvm_typed_record_data_ptr,
                    boxed::Record::EXTERNAL_INLINE_LEN as usize,
                )
            }
        };

        let inline_byte_len_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::IS_INLINE_INDEX,
            libcstr!("inline_byte_len_ptr"),
        );
        let llvm_inline_byte_len = LLVMConstInt(llvm_i8, inline_byte_len as u64, 1);
        LLVMBuildStore(builder, llvm_inline_byte_len, inline_byte_len_ptr);

        for (field_index, llvm_field) in llvm_fields.iter().enumerate() {
            let field_gep_indices = &mut [
                LLVMConstInt(llvm_i32, 0, 0),
                LLVMConstInt(llvm_i32, field_index as u64, 0),
            ];

            let llvm_field_ptr = LLVMBuildInBoundsGEP(
                builder,
                llvm_record_data_ptr,
                field_gep_indices.as_mut_ptr(),
                field_gep_indices.len() as u32,
                libcstr!("init_record_field_ptr"),
            );

            LLVMBuildStore(builder, *llvm_field, llvm_field_ptr);
        }

        let boxed_record_name = format!("alloced_{}_record\0", record_struct.source_name);

        LLVMBuildBitCast(
            builder,
            alloced_boxed_record,
            tcx.boxed_abi_to_llvm_ptr_type(&boxed::TypeTag::Record.into()),
            boxed_record_name.as_ptr() as *const _,
        )
    }
}
