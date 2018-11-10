use std::collections::HashMap;
use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::mir::ops;

use crate::codegen::analysis::escape::{CaptureKind, Captures};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::codegen::GenABI;

pub struct GenedFun {
    pub llvm_value: LLVMValueRef,
    pub takes_task: bool,
}

pub(crate) struct FunCtx {
    pub regs: HashMap<ops::RegId, LLVMValueRef>,
    pub current_task: Option<LLVMValueRef>,

    pub function: LLVMValueRef,
    pub builder: LLVMBuilderRef,
}

impl FunCtx {
    pub(crate) fn new(function: LLVMValueRef, builder: LLVMBuilderRef) -> FunCtx {
        FunCtx {
            regs: HashMap::new(),
            current_task: None,

            function,
            builder,
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

pub(crate) fn gen_fun(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_>,
    fun: &ops::Fun,
    captures: &Captures,
) -> GenedFun {
    use crate::codegen::alloc::plan::plan_allocs;
    use crate::codegen::analysis;
    use crate::codegen::op_gen;
    use runtime::abitype::{ABIType, ParamABIType, RetABIType};

    // Use the capture information to plan our allocations
    let alloc_plan = plan_allocs(&captures, &fun.ops);

    // Use the allocation plan to determine if we need a task parameter
    let takes_task = fun.abi.external_call_conv
        || analysis::needs_task::alloc_plan_needs_task(tcx, mcx, &alloc_plan);

    // Determine which params we captured
    let param_captures: Vec<CaptureKind> = fun
        .params
        .iter()
        .map(|param_reg| captures.get(*param_reg))
        .collect();

    let gen_abi = GenABI {
        takes_task,
        params: fun
            .abi
            .params
            .iter()
            .map(|abi_type| abi_type.clone().into())
            .collect::<Vec<ParamABIType>>()
            .into_boxed_slice(),
        ret: fun.abi.ret.clone(),
    };

    unsafe {
        let builder = LLVMCreateBuilderInContext(tcx.llx);

        let function_type = tcx.fun_abi_to_llvm_type(&gen_abi);

        let fun_symbol = fun
            .source_name
            .as_ref()
            .map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap())
            .unwrap_or_else(|| ffi::CString::new("anon_fun").unwrap());

        let function = LLVMAddFunction(mcx.module, fun_symbol.as_ptr() as *const _, function_type);

        let bb = LLVMAppendBasicBlockInContext(tcx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut fcx = FunCtx::new(function, builder);

        if takes_task {
            fcx.current_task = Some(LLVMGetParam(function, 0));
        }

        let llvm_params_offset = takes_task as usize;
        for (param_index, reg) in fun.params.iter().enumerate() {
            let llvm_offset = (llvm_params_offset + param_index) as u32;
            fcx.regs.insert(*reg, LLVMGetParam(function, llvm_offset));
        }

        // Our task is an implicit parameter
        for (param_index, param_abi_type) in fun.abi.params.iter().enumerate() {
            if let ABIType::Boxed(_) = param_abi_type {
                let no_capture = param_captures[param_index] == CaptureKind::Never;

                tcx.add_boxed_param_attrs(
                    function,
                    (llvm_params_offset + param_index + 1) as u32,
                    no_capture,
                );
            }
        }

        if let RetABIType::Inhabited(ABIType::Boxed(_)) = fun.abi.ret {
            tcx.add_boxed_return_attrs(function);
        }

        for alloc_atom in alloc_plan {
            op_gen::gen_alloc_atom(tcx, mcx, &mut fcx, &alloc_atom);
        }

        mcx.optimise_function(function);

        GenedFun {
            llvm_value: function,
            takes_task,
        }
    }
}
