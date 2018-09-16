use std::collections::HashMap;
use std::ffi::CString;
use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::CodegenCtx;
use crate::mir::ops::*;

pub(crate) struct FunCtx {
    regs: HashMap<RegId, LLVMValueRef>,
}

impl FunCtx {
    pub(crate) fn new() -> FunCtx {
        FunCtx {
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

pub(crate) fn gen_op(
    cgx: &mut CodegenCtx,
    fcx: &mut FunCtx,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    op: &Op,
) {
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

                let llvm_value = cgx.get_global_or_insert(module, llvm_type, &box_name, |cgx| {
                    let mut members = vec![];
                    push_box_header(cgx, &mut members, type_tag);
                    members.push(LLVMConstInt(llvm_i64, *value as u64, 1));

                    LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
                });
                LLVMSetAlignment(llvm_value, mem::align_of::<boxed::Int>() as u32);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstEntryPoint(reg, ConstEntryPointOp { symbol, abi }) => {
                let function_type =
                    cgx.function_to_llvm_type(abi.takes_task, &abi.params, &abi.ret);

                let function = LLVMAddGlobal(
                    module,
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
                    builder,
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
