use std::{ffi, mem};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMLinkage, LLVMUnnamedAddr};

use runtime::boxed;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;

fn annotate_private_global(llvm_global: LLVMValueRef) {
    unsafe {
        LLVMSetUnnamedAddress(llvm_global, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
        LLVMSetGlobalConstant(llvm_global, 1 as i32);
        LLVMSetLinkage(llvm_global, LLVMLinkage::LLVMPrivateLinkage)
    }
}

pub fn gen_boxed_pair(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    llvm_head: LLVMValueRef,
    llvm_rest: LLVMValueRef,
    llvm_length: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::TopPair;
        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let members = &mut [
            cgx.llvm_box_header(type_tag.into_const_header()),
            llvm_length,
            llvm_head,
            llvm_rest,
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, "const_pair\0".as_ptr() as *const _);
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::TopPair>() as u32);

        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_inline_str(cgx: &mut CodegenCtx, mcx: &mut ModCtx, value: &str) -> LLVMValueRef {
    unsafe {
        const MAX_INLINE_BYTES: usize = boxed::Str::MAX_INLINE_BYTES;

        let mut inline_buffer: [u8; MAX_INLINE_BYTES] = [0; MAX_INLINE_BYTES];
        inline_buffer[0..value.len()].copy_from_slice(value.as_bytes());

        let type_tag = boxed::TypeTag::Str;
        let inline_llvm_type = cgx.boxed_inline_str_llvm_type();
        let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

        let members = &mut [
            cgx.llvm_box_header(type_tag.into_const_header()),
            LLVMConstInt(llvm_i8, value.len() as u64, 0),
            LLVMConstStringInContext(
                cgx.llx,
                inline_buffer.as_mut_ptr() as *mut _,
                MAX_INLINE_BYTES as u32,
                1,
            ),
        ];

        let inline_llvm_value =
            LLVMConstNamedStruct(inline_llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(
            mcx.module,
            inline_llvm_type,
            "const_str\0".as_ptr() as *const _,
        );
        LLVMSetInitializer(global, inline_llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Str>() as u32);
        annotate_private_global(global);

        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        LLVMConstBitCast(global, LLVMPointerType(llvm_type, 0))
    }
}

pub fn gen_boxed_int(cgx: &mut CodegenCtx, mcx: &mut ModCtx, value: i64) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::Int;
        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);

        let box_name = ffi::CString::new(format!("const_int_{}", value)).unwrap();

        let global = mcx.get_global_or_insert(llvm_type, &box_name, || {
            let members = &mut [
                cgx.llvm_box_header(type_tag.into_const_header()),
                LLVMConstInt(llvm_i64, value as u64, 1),
            ];

            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
        });

        LLVMSetAlignment(global, mem::align_of::<boxed::Int>() as u32);
        annotate_private_global(global);
        global
    }
}

pub fn gen_boxed_fun_thunk(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    llvm_entry_point: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::FunThunk;
        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());

        let members = &mut [
            cgx.llvm_box_header(type_tag.into_const_header()),
            LLVMConstNull(cgx.record_llvm_type()),
            llvm_entry_point,
        ];

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(
            mcx.module,
            llvm_type,
            "const_fun_thunk\0".as_ptr() as *const _,
        );
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::FunThunk>() as u32);

        annotate_private_global(global);
        global
    }
}
