use std::ffi::CString;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::codegen::{CodegenCtx, FunCtx};
use crate::mir::ops::*;

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
