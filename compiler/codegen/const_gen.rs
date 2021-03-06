use std::rc::Rc;
use std::{iter, mem};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMLinkage, LLVMUnnamedAddr};

use arret_runtime::boxed;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::libcstr;
use crate::mir::ops;

pub fn annotate_private_global(llvm_global: LLVMValueRef) {
    unsafe {
        LLVMSetUnnamedAddress(llvm_global, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
        LLVMSetGlobalConstant(llvm_global, 1);
        LLVMSetLinkage(llvm_global, LLVMLinkage::LLVMPrivateLinkage)
    }
}

pub fn gen_boxed_pair(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_head: LLVMValueRef,
    llvm_rest: LLVMValueRef,
    llvm_list_len: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::Pair;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            llvm_list_len,
            llvm_head,
            llvm_rest,
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_pair"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Pair>() as u32);

        annotate_private_global(global);
        global
    }
}

fn gen_boxed_external_str(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: &str,
) -> LLVMValueRef {
    unsafe {
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let shared_str_members = &mut [
            // ref_count
            LLVMConstInt(llvm_i64, std::u64::MAX, 0),
            // len
            LLVMConstInt(llvm_i64, value.len() as u64, 0),
            // data
            LLVMConstStringInContext(tcx.llx, value.as_ptr() as *mut _, value.len() as u32, 1),
        ];

        let shared_str_llvm_value = LLVMConstStructInContext(
            tcx.llx,
            shared_str_members.as_mut_ptr(),
            shared_str_members.len() as u32,
            0,
        );

        let shared_str_global = LLVMAddGlobal(
            mcx.module,
            LLVMTypeOf(shared_str_llvm_value),
            libcstr!("shared_str"),
        );
        LLVMSetInitializer(shared_str_global, shared_str_llvm_value);
        annotate_private_global(shared_str_global);

        let type_tag = boxed::TypeTag::Str;
        let external_llvm_type = tcx.boxed_external_str_llvm_type();
        let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);

        let external_members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(llvm_i8, boxed::Str::EXTERNAL_INLINE_BYTE_LEN as u64, 0),
            LLVMConstBitCast(
                shared_str_global,
                LLVMPointerType(tcx.shared_str_llvm_type(), 0),
            ),
        ];

        let external_llvm_value = LLVMConstNamedStruct(
            external_llvm_type,
            external_members.as_mut_ptr(),
            external_members.len() as u32,
        );

        let global = LLVMAddGlobal(mcx.module, external_llvm_type, libcstr!("const_str"));
        LLVMSetInitializer(global, external_llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Str>() as u32);
        annotate_private_global(global);

        LLVMConstBitCast(global, tcx.boxed_abi_to_llvm_ptr_type(&type_tag.into()))
    }
}

fn gen_boxed_inline_str(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: &str,
) -> LLVMValueRef {
    unsafe {
        const MAX_INLINE_BYTES: usize = boxed::Str::MAX_INLINE_BYTES;

        let mut inline_buffer: [u8; MAX_INLINE_BYTES] = [0; MAX_INLINE_BYTES];
        inline_buffer[0..value.len()].copy_from_slice(value.as_bytes());

        let type_tag = boxed::TypeTag::Str;
        let inline_llvm_type = tcx.boxed_inline_str_llvm_type();
        let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);

        let members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(llvm_i8, value.len() as u64, 0),
            LLVMConstStringInContext(
                tcx.llx,
                inline_buffer.as_mut_ptr() as *mut _,
                MAX_INLINE_BYTES as u32,
                1,
            ),
        ];

        let inline_llvm_value =
            LLVMConstNamedStruct(inline_llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, inline_llvm_type, libcstr!("const_str"));
        LLVMSetInitializer(global, inline_llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Str>() as u32);
        annotate_private_global(global);

        LLVMConstBitCast(global, tcx.boxed_abi_to_llvm_ptr_type(&type_tag.into()))
    }
}

pub fn gen_boxed_str(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: &str,
) -> LLVMValueRef {
    match boxed::Str::storage_for_byte_len(value.len()) {
        boxed::StrStorage::Inline(_) => gen_boxed_inline_str(tcx, mcx, value),
        boxed::StrStorage::External => gen_boxed_external_str(tcx, mcx, value),
    }
}

pub fn gen_boxed_sym(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: &str,
) -> LLVMValueRef {
    let interned_sym = mcx.intern_name(value);

    unsafe {
        let type_tag = boxed::TypeTag::Sym;
        let boxed_llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(llvm_i64, interned_sym.to_raw_u64(), 0),
        ];
        let boxed_llvm_value =
            LLVMConstNamedStruct(boxed_llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, boxed_llvm_type, libcstr!("const_sym"));
        LLVMSetInitializer(global, boxed_llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Sym>() as u32);
        annotate_private_global(global);

        global
    }
}

/// Generates a table of global interned names
///
/// `names` must be pre-sorted
pub fn gen_global_interned_names<'a>(
    tcx: &mut TargetCtx,
    llvm_module: LLVMModuleRef,
    names: impl ExactSizeIterator<Item = &'a Rc<str>>,
) -> LLVMValueRef {
    unsafe {
        let names_len = names.len();
        if names_len == 0 {
            return LLVMConstPointerNull(LLVMPointerType(tcx.global_interned_name_llvm_type(), 0));
        }

        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let global_interned_name_llvm_type = tcx.global_interned_name_llvm_type();

        let first_element_gep_indices =
            &mut [LLVMConstInt(llvm_i32, 0, 0), LLVMConstInt(llvm_i32, 0, 0)];

        let mut llvm_names: Vec<LLVMValueRef> = names
            .map(|name| {
                let llvm_name_string = LLVMConstStringInContext(
                    tcx.llx,
                    name.as_bytes().as_ptr() as *mut _,
                    name.len() as u32,
                    1,
                );

                let name_global_name = format!("global_interned_name_{}\0", name);

                let llvm_name_global = LLVMAddGlobal(
                    llvm_module,
                    LLVMTypeOf(llvm_name_string),
                    name_global_name.as_ptr() as *const _,
                );
                LLVMSetInitializer(llvm_name_global, llvm_name_string);
                annotate_private_global(llvm_name_global);

                let llvm_name_string_ptr = LLVMConstGEP(
                    llvm_name_global,
                    first_element_gep_indices.as_mut_ptr(),
                    first_element_gep_indices.len() as u32,
                );

                let llvm_name_members = &mut [
                    LLVMConstInt(llvm_i64, name.len() as u64, 0),
                    llvm_name_string_ptr,
                ];

                LLVMConstNamedStruct(
                    global_interned_name_llvm_type,
                    llvm_name_members.as_mut_ptr(),
                    llvm_name_members.len() as u32,
                )
            })
            .collect();

        let llvm_names_array = LLVMConstArray(
            global_interned_name_llvm_type,
            llvm_names.as_mut_ptr(),
            llvm_names.len() as u32,
        );

        let global_names_members = &mut [
            // len
            LLVMConstInt(llvm_i32, names_len as u64, 0),
            // names
            llvm_names_array,
        ];

        let llvm_global_names = LLVMConstStructInContext(
            tcx.llx,
            global_names_members.as_mut_ptr(),
            global_names_members.len() as u32,
            0,
        );

        let global = LLVMAddGlobal(
            llvm_module,
            LLVMTypeOf(llvm_global_names),
            libcstr!("global_interned_names"),
        );

        LLVMSetInitializer(global, llvm_global_names);
        annotate_private_global(global);

        global
    }
}

pub fn gen_boxed_int(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: i64,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::Int;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let box_name = format!("const_int_{}\0", value);

        let global = mcx.get_global_or_insert(llvm_type, box_name.as_bytes(), || {
            let members = &mut [
                tcx.llvm_box_header(type_tag.to_const_header()),
                LLVMConstInt(llvm_i64, value as u64, 1),
            ];

            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
        });

        LLVMSetAlignment(global, mem::align_of::<boxed::Int>() as u32);
        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_float(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: f64,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::Float;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_double = LLVMDoubleTypeInContext(tcx.llx);

        let members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstReal(llvm_double, value),
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_float"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Float>() as u32);

        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_char(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    value: char,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::Char;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);

        let box_name = format!("const_char_{}\0", value);

        let global = mcx.get_global_or_insert(llvm_type, box_name.as_bytes(), || {
            let members = &mut [
                tcx.llvm_box_header(type_tag.to_const_header()),
                LLVMConstInt(llvm_i32, value as u64, 1),
            ];

            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
        });

        LLVMSetAlignment(global, mem::align_of::<boxed::Char>() as u32);
        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_nil(tcx: &mut TargetCtx, mcx: &mut ModCtx<'_, '_, '_>) -> LLVMValueRef {
    tcx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::Nil, b"ARRET_NIL\0")
}

pub fn gen_boxed_fun_thunk(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_captures: LLVMValueRef,
    llvm_entry_point: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::FunThunk;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let members = &mut [
            tcx.llvm_box_header(type_tag.to_const_header()),
            llvm_captures,
            llvm_entry_point,
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_fun_thunk"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::FunThunk>() as u32);

        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_record(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    record_struct: &ops::RecordStructId,
    llvm_fields: &[LLVMValueRef],
) -> LLVMValueRef {
    let type_tag = boxed::TypeTag::Record;
    let record_class_id = mcx.record_class_id_for_struct(record_struct);

    let record_struct::TargetRecordStruct {
        data_layout,
        record_storage,
        llvm_data_type,
        ..
    } = *tcx.target_record_struct(record_struct);

    let llvm_box_type = tcx.record_struct_llvm_box_type(record_struct);

    unsafe {
        let box_name = format!("const_{}\0", record_struct.source_name);

        let llvm_data_struct = LLVMConstNamedStruct(
            llvm_data_type,
            llvm_fields.as_ptr() as *mut _,
            llvm_fields.len() as u32,
        );

        let llvm_box_header = tcx.llvm_box_header(type_tag.to_const_header());

        // Constant records by definition cannot have GC refs
        let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
        let llvm_has_gc_refs = LLVMConstInt(llvm_i8, 0, 1);

        let llvm_record_class_id = LLVMConstInt(
            tcx.record_class_id_llvm_type(),
            u64::from(record_class_id),
            1,
        );

        let llvm_box_value = if let boxed::RecordStorage::Inline(_) = record_storage {
            let llvm_inline_byte_len = LLVMConstInt(
                llvm_i8,
                match data_layout {
                    Some(data_layout) => data_layout.size() as u64,
                    None => 0,
                },
                1,
            );

            let inline_box_members = &mut [
                llvm_box_header,
                llvm_inline_byte_len,
                llvm_has_gc_refs,
                llvm_record_class_id,
                llvm_data_struct,
            ];

            LLVMConstNamedStruct(
                llvm_box_type,
                inline_box_members.as_mut_ptr(),
                inline_box_members.len() as u32,
            )
        } else {
            // Create a global containing our data and return a pointer to it
            let data_global_name = format!("const_{}_data\0", record_struct.source_name);

            let data_global = LLVMAddGlobal(
                mcx.module,
                llvm_data_type,
                data_global_name.as_ptr() as *const _,
            );
            LLVMSetInitializer(data_global, llvm_data_struct);
            annotate_private_global(data_global);

            let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);
            let external_box_members = &mut [
                llvm_box_header,
                LLVMConstInt(llvm_i8, boxed::Record::EXTERNAL_INLINE_LEN as u64, 1),
                llvm_has_gc_refs,
                llvm_record_class_id,
                data_global,
                LLVMConstInt(llvm_i64, 0, 0),
            ];

            LLVMConstNamedStruct(
                llvm_box_type,
                external_box_members.as_mut_ptr(),
                external_box_members.len() as u32,
            )
        };

        let global = LLVMAddGlobal(mcx.module, llvm_box_type, box_name.as_ptr() as *const _);
        LLVMSetInitializer(global, llvm_box_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Record>() as u32);
        annotate_private_global(global);

        LLVMConstBitCast(global, tcx.boxed_abi_to_llvm_ptr_type(&type_tag.into()))
    }
}

fn gen_persistent_vector_leaf(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements: &[LLVMValueRef],
) -> LLVMValueRef {
    use arret_runtime::abitype::BoxedAbiType;
    use arret_runtime::persistent::vector::GLOBAL_CONSTANT_REFCOUNT;
    use arret_runtime::persistent::vector::NODE_SIZE;

    unsafe {
        let llvm_type = tcx.persistent_vector_leaf_llvm_type();
        let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let mut padded_llvm_elements: Vec<LLVMValueRef> = llvm_elements
            .iter()
            .copied()
            .chain(iter::repeat(LLVMGetUndef(llvm_any_ptr)).take(NODE_SIZE - llvm_elements.len()))
            .collect();

        let mut members = vec![
            LLVMConstInt(llvm_i64, GLOBAL_CONSTANT_REFCOUNT as u64, 0),
            LLVMConstArray(
                llvm_any_ptr,
                padded_llvm_elements.as_mut_ptr(),
                padded_llvm_elements.len() as u32,
            ),
        ];

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_vector_leaf"));

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        LLVMSetInitializer(global, llvm_value);
        annotate_private_global(global);
        global
    }
}

fn gen_boxed_external_vector(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements_iter: impl ExactSizeIterator<Item = LLVMValueRef>,
) -> LLVMValueRef {
    use arret_runtime::persistent::vector::NODE_SIZE;

    let llvm_elements: Vec<LLVMValueRef> = llvm_elements_iter.collect();

    unsafe {
        let type_tag = boxed::TypeTag::Vector;
        let llvm_type = tcx.boxed_external_vector_llvm_type();

        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);
        let llvm_persistent_vector_leaf_ptr =
            LLVMPointerType(tcx.persistent_vector_leaf_llvm_type(), 0);

        let (root_ptr, tail_ptr) = if llvm_elements.len() > NODE_SIZE {
            // Need a root a tail
            (
                gen_persistent_vector_leaf(tcx, mcx, &llvm_elements[0..NODE_SIZE]),
                gen_persistent_vector_leaf(tcx, mcx, &llvm_elements[NODE_SIZE..]),
            )
        } else {
            // Need just the tail
            (
                LLVMConstPointerNull(llvm_persistent_vector_leaf_ptr),
                gen_persistent_vector_leaf(tcx, mcx, &llvm_elements),
            )
        };

        let mut members = [
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(
                llvm_i32,
                boxed::Vector::<boxed::Any>::EXTERNAL_INLINE_LEN as u64,
                0,
            ),
            LLVMConstInt(llvm_i64, llvm_elements.len() as u64, 0),
            root_ptr,
            tail_ptr,
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_vector"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Vector>() as u32);

        annotate_private_global(global);
        global
    }
}

fn gen_boxed_inline_vector(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements: impl ExactSizeIterator<Item = LLVMValueRef>,
) -> LLVMValueRef {
    use arret_runtime::abitype::BoxedAbiType;

    let elements_len = llvm_elements.len();

    unsafe {
        let type_tag = boxed::TypeTag::Vector;
        let llvm_type = tcx.boxed_inline_vector_llvm_type();
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);

        let mut members: Vec<LLVMValueRef> = vec![
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(llvm_i32, elements_len as u64, 0),
        ];

        members.extend(
            llvm_elements.chain(
                iter::repeat(LLVMGetUndef(llvm_any_ptr))
                    .take(boxed::Vector::<boxed::Any>::MAX_INLINE_LEN - elements_len),
            ),
        );

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_vector"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Vector>() as u32);

        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_vector(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements: impl ExactSizeIterator<Item = LLVMValueRef>,
) -> LLVMValueRef {
    use arret_runtime::persistent::vector::NODE_SIZE;

    let elements_len = llvm_elements.len();

    if elements_len <= boxed::Vector::<boxed::Any>::MAX_INLINE_LEN {
        gen_boxed_inline_vector(tcx, mcx, llvm_elements)
    } else if elements_len <= (NODE_SIZE * 2) {
        gen_boxed_external_vector(tcx, mcx, llvm_elements)
    } else {
        todo!("generating constant vector of length {}", elements_len);
    }
}

pub fn gen_boxed_set(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements: impl ExactSizeIterator<Item = LLVMValueRef>,
) -> LLVMValueRef {
    use arret_runtime::abitype::BoxedAbiType;

    let elements_len = llvm_elements.len();

    if elements_len > boxed::Set::<boxed::Any>::MAX_INLINE_LEN {
        todo!("generating constant set of length {}", elements_len);
    }

    unsafe {
        let type_tag = boxed::TypeTag::Set;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
        let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);

        let mut members: Vec<LLVMValueRef> = vec![
            tcx.llvm_box_header(type_tag.to_const_header()),
            LLVMConstInt(llvm_i32, elements_len as u64, 0),
        ];

        members.extend(
            llvm_elements.chain(
                iter::repeat(LLVMGetUndef(llvm_any_ptr))
                    .take(boxed::Set::<boxed::Any>::MAX_INLINE_LEN - elements_len),
            ),
        );

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_set"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Set>() as u32);

        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_map(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    llvm_elements: impl ExactSizeIterator<Item = (LLVMValueRef, LLVMValueRef)>,
) -> LLVMValueRef {
    if llvm_elements.len() > 0 {
        todo!("generating non-empty map");
    }

    unsafe {
        let type_tag = boxed::TypeTag::Map;
        let llvm_type = tcx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let mut members: Vec<LLVMValueRef> = vec![tcx.llvm_box_header(type_tag.to_const_header())];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, libcstr!("const_map"));
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Map>() as u32);

        annotate_private_global(global);
        global
    }
}
