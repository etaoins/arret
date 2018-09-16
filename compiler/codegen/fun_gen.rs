use std::collections::HashMap;
use std::ffi::CString;
use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops::*;

pub(crate) struct FunCtx {
    pub builder: LLVMBuilderRef,
    regs: HashMap<RegId, LLVMValueRef>,
}

impl FunCtx {
    pub(crate) fn new(builder: LLVMBuilderRef) -> FunCtx {
        FunCtx {
            builder,
            regs: HashMap::new(),
        }
    }
}

fn push_box_header(
    cgx: &mut CodegenCtx,
    members: &mut Vec<LLVMValueRef>,
    type_tag: boxed::TypeTag,
) {
    unsafe {
        let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

        members.push(LLVMConstInt(llvm_i8, 0, 0));
        members.push(LLVMConstInt(llvm_i8, type_tag as u64, 0));
    }
}

pub(crate) fn gen_op(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fcx: &mut FunCtx, op: &Op) {
    unsafe {
        match &op.kind {
            OpKind::ConstInt(reg, value) => {
                let llvm_val = LLVMConstInt(LLVMInt64TypeInContext(cgx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_val);
            }
            OpKind::ConstBoxedInt(reg, value) => {
                let type_tag = boxed::TypeTag::Int;
                let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
                let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);

                let box_name = CString::new(format!("arret_int_{}", value)).unwrap();

                let llvm_value = mcx.get_global_or_insert(llvm_type, &box_name, || {
                    let mut members = vec![];
                    push_box_header(cgx, &mut members, type_tag);
                    members.push(LLVMConstInt(llvm_i64, *value as u64, 1));

                    LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
                });

                LLVMSetAlignment(llvm_value, mem::align_of::<boxed::Int>() as u32);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedStr(reg, value) => {
                const MAX_INLINE_BYTES: usize = boxed::Str::MAX_INLINE_BYTES;

                if value.len() > MAX_INLINE_BYTES {
                    unimplemented!("Non-inline strings");
                }

                let mut inline_buffer: [u8; MAX_INLINE_BYTES] = [0; MAX_INLINE_BYTES];
                inline_buffer[0..value.len()].copy_from_slice(value.as_bytes());

                let type_tag = boxed::TypeTag::Str;
                let inline_llvm_type = cgx.boxed_inline_str_llvm_type();
                let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

                let mut members = vec![];
                push_box_header(cgx, &mut members, type_tag);
                members.push(LLVMConstInt(llvm_i8, value.len() as u64, 0));
                members.push(LLVMConstString(
                    inline_buffer.as_mut_ptr() as *mut _,
                    MAX_INLINE_BYTES as u32,
                    1,
                ));

                let inline_llvm_value = LLVMConstNamedStruct(
                    inline_llvm_type,
                    members.as_mut_ptr(),
                    members.len() as u32,
                );

                let global = LLVMAddGlobal(
                    mcx.module,
                    inline_llvm_type,
                    "arret_str\0".as_ptr() as *const _,
                );
                LLVMSetInitializer(global, inline_llvm_value);
                LLVMSetAlignment(global, mem::align_of::<boxed::Str>() as u32);

                let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
                let llvm_value = LLVMConstBitCast(global, LLVMPointerType(llvm_type, 0));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstEntryPoint(reg, ConstEntryPointOp { symbol, abi }) => {
                let function_type =
                    cgx.function_to_llvm_type(abi.takes_task, &abi.params, &abi.ret);

                let function = LLVMAddGlobal(
                    mcx.module,
                    function_type,
                    CString::new(*symbol).unwrap().as_bytes_with_nul().as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, function);
            }
            OpKind::Call(reg, CallOp { fun_reg, args }) => {
                let llvm_fun = fcx.regs[&fun_reg];
                let mut llvm_args = args
                    .iter()
                    .map(|param_reg| fcx.regs[&param_reg])
                    .collect::<Vec<LLVMValueRef>>();

                let llvm_ret = LLVMBuildCall(
                    fcx.builder,
                    llvm_fun,
                    llvm_args.as_mut_ptr(),
                    llvm_args.len() as u32,
                    b"\0".as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, llvm_ret);
            }
        }
    }
}
