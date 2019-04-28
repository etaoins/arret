use std::collections::HashMap;
use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::mir::ops;

use crate::codegen::analysis::escape::{CaptureKind, Captures};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::codegen::GenABI;

pub(crate) struct FunCtx {
    pub regs: HashMap<ops::RegId, LLVMValueRef>,

    pub function: LLVMValueRef,
    pub builder: LLVMBuilderRef,
    pub current_task: LLVMValueRef,
}

impl FunCtx {
    pub(crate) fn new(
        function: LLVMValueRef,
        builder: LLVMBuilderRef,
        current_task: LLVMValueRef,
    ) -> FunCtx {
        FunCtx {
            regs: HashMap::new(),

            function,
            builder,
            current_task,
        }
    }
}

impl Drop for FunCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

pub(crate) fn declare_fun(
    tcx: &mut TargetCtx,
    llvm_module: LLVMModuleRef,
    fun: &ops::Fun,
) -> LLVMValueRef {
    let gen_abi = GenABI {
        takes_task: true,
        params: fun
            .abi
            .params
            .iter()
            .map(|abi_type| abi_type.clone().into())
            .collect(),
        ret: fun.abi.ret.clone(),
    };

    let function_type = tcx.fun_abi_to_llvm_type(&gen_abi);

    let fun_symbol = fun
        .source_name
        .as_ref()
        .map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap())
        .unwrap_or_else(|| ffi::CString::new("anon_fun").unwrap());

    unsafe { LLVMAddFunction(llvm_module, fun_symbol.as_ptr() as *const _, function_type) }
}

pub(crate) fn define_fun(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fun: &ops::Fun,
    captures: &Captures,
    llvm_fun: LLVMValueRef,
) {
    use crate::codegen::alloc::plan::plan_allocs;
    use crate::codegen::op_gen;
    use arret_runtime::abitype::{ABIType, RetABIType};

    let alloc_plan = plan_allocs(&captures, &fun.ops);

    unsafe {
        let builder = LLVMCreateBuilderInContext(tcx.llx);
        let bb = LLVMAppendBasicBlockInContext(tcx.llx, llvm_fun, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut fcx = FunCtx::new(llvm_fun, builder, LLVMGetParam(llvm_fun, 0));
        fcx.regs.reserve(fun.param_regs.len());

        for (param_index, (reg, param_abi_type)) in
            fun.param_regs.iter().zip(fun.abi.params.iter()).enumerate()
        {
            // Our implicit task param shifts our params by 1
            let llvm_offset = (1 + param_index) as u32;
            fcx.regs.insert(*reg, LLVMGetParam(llvm_fun, llvm_offset));

            if let ABIType::Boxed(_) = param_abi_type {
                let no_capture = captures.get(*reg) == CaptureKind::Never;
                tcx.add_boxed_param_attrs(llvm_fun, llvm_offset, no_capture);
            }
        }

        if let RetABIType::Inhabited(ABIType::Boxed(_)) = fun.abi.ret {
            tcx.add_boxed_return_attrs(llvm_fun);
        }

        for alloc_atom in alloc_plan {
            op_gen::gen_alloc_atom(tcx, mcx, &mut fcx, alloc_atom);
        }

        mcx.optimise_function(llvm_fun);
    }
}
