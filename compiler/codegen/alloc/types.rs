use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use arret_runtime::boxed;

use crate::codegen::alloc::core::{gen_alloced_box, gen_alloced_box_with_llvm_type};
use crate::codegen::alloc::{ActiveAlloc, BoxSource};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops::RecordStructId;

pub struct PairInput {
    pub llvm_head: LLVMValueRef,
    pub llvm_rest: LLVMValueRef,
    pub llvm_length: LLVMValueRef,
}

pub struct FunThunkInput {
    pub llvm_closure: LLVMValueRef,
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

        let interned_sym_ptr = LLVMBuildStructGEP(
            builder,
            alloced_sym,
            1,
            b"interned_sym_ptr\0".as_ptr() as *const _,
        );
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

        let contains_gc_refs = record_struct
            .field_abi_types
            .iter()
            .zip(llvm_fields.iter())
            .any(|(field_abi_type, llvm_field)| {
                field_abi_type.may_contain_gc_refs() && LLVMIsConstant(*llvm_field) == 0
            });

        let boxed_record_name =
            ffi::CString::new(format!("alloced_{}_record", record_struct.source_name)).unwrap();

        let llvm_box_type = tcx.record_struct_box_type(record_struct);
        let alloced_boxed_record = gen_alloced_box_with_llvm_type::<boxed::Record>(
            tcx,
            builder,
            active_alloc,
            box_source,
            llvm_box_type,
            boxed_record_name.as_bytes_with_nul(),
        );

        let contains_gc_refs_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::CONTAINS_GC_REFS_INDEX,
            b"contains_gc_refs_ptr\0".as_ptr() as *const _,
        );
        let llvm_contains_gc_refs = LLVMConstInt(llvm_i8, contains_gc_refs as u64, 1);
        LLVMBuildStore(builder, llvm_contains_gc_refs, contains_gc_refs_ptr);

        let record_class_id_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::RECORD_CLASS_ID_INDEX,
            b"record_class_id_ptr\0".as_ptr() as *const _,
        );

        let llvm_record_class_id = LLVMConstInt(
            tcx.record_class_id_llvm_type(),
            u64::from(record_class_id),
            1,
        );
        LLVMBuildStore(builder, llvm_record_class_id, record_class_id_ptr);

        // This is used by both inline and external records
        let record_data_gep_indices = &mut [
            LLVMConstInt(llvm_i32, 0 as u64, 0),
            LLVMConstInt(llvm_i32, u64::from(record_struct::DATA_INDEX), 0),
        ];

        let (llvm_record_data_ptr, inline_byte_len) = match (record_storage, box_source) {
            (boxed::RecordStorage::Inline(_), _) => {
                let llvm_inline_record_data_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_data_gep_indices.as_mut_ptr(),
                    record_data_gep_indices.len() as u32,
                    "inline_record_data\0".as_ptr() as *const _,
                );

                (llvm_inline_record_data_ptr, data_layout.size())
            }
            (boxed::RecordStorage::External, BoxSource::Stack) => {
                // Allocate the record data
                let llvm_stack_record_data_ptr = LLVMBuildAlloca(
                    builder,
                    llvm_data_type,
                    "stack_record_data\0".as_ptr() as *const _,
                );

                // Update our record data pointer
                let llvm_record_data_ptr_ptr = LLVMBuildInBoundsGEP(
                    builder,
                    alloced_boxed_record,
                    record_data_gep_indices.as_mut_ptr(),
                    record_data_gep_indices.len() as u32,
                    "record_data_ptr_ptr\0".as_ptr() as *const _,
                );

                LLVMBuildStore(
                    builder,
                    llvm_stack_record_data_ptr,
                    llvm_record_data_ptr_ptr,
                );

                (
                    llvm_stack_record_data_ptr,
                    boxed::Record::MAX_INLINE_BYTES + 1,
                )
            }
            (boxed::RecordStorage::External, BoxSource::Heap(_)) => {
                unimplemented!("allocating external boxed records on heap");
            }
        };

        let inline_byte_length_ptr = LLVMBuildStructGEP(
            builder,
            alloced_boxed_record,
            record_struct::IS_INLINE_INDEX,
            b"inline_byte_length_ptr\0".as_ptr() as *const _,
        );
        let llvm_inline_byte_length = LLVMConstInt(llvm_i8, inline_byte_len as u64, 1);
        LLVMBuildStore(builder, llvm_inline_byte_length, inline_byte_length_ptr);

        for (field_index, llvm_field) in llvm_fields.iter().enumerate() {
            let field_gep_indices = &mut [
                LLVMConstInt(llvm_i32, 0 as u64, 0),
                LLVMConstInt(llvm_i32, field_index as u64, 0),
            ];

            let llvm_field_ptr = LLVMBuildInBoundsGEP(
                builder,
                llvm_record_data_ptr,
                field_gep_indices.as_mut_ptr(),
                field_gep_indices.len() as u32,
                b"init_record_field_ptr\0".as_ptr() as *const _,
            );

            LLVMBuildStore(builder, *llvm_field, llvm_field_ptr);
        }

        let boxed_record_name =
            ffi::CString::new(format!("alloced_{}_record", record_struct.source_name)).unwrap();

        LLVMBuildBitCast(
            builder,
            alloced_boxed_record,
            tcx.boxed_abi_to_llvm_ptr_type(&boxed::TypeTag::Record.into()),
            boxed_record_name.to_bytes_with_nul().as_ptr() as *const _,
        )
    }
}
