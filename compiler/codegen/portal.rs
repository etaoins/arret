use std::os::raw::c_char;
use std::{mem, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;

use runtime;
use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::codegen::convert::convert_to_boxed_any;
use crate::codegen::CodegenCtx;
use crate::hir::rfi;

type PortalEntryPoint = fn(&mut runtime::task::Task, Gc<boxed::Any>) -> Gc<boxed::Any>;

pub struct Portal {
    execution_engine: *mut LLVMOpaqueExecutionEngine,
    entry_point: PortalEntryPoint,
}

impl Portal {
    pub fn entry_point(&self) -> PortalEntryPoint {
        self.entry_point
    }
}

impl Drop for Portal {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeExecutionEngine(self.execution_engine);
        }
    }
}

pub fn create_portal_for_rust_fun(cgx: &mut CodegenCtx, rust_fun: &rfi::Fun) -> Portal {
    unsafe {
        // Create the module
        let module = LLVMModuleCreateWithNameInContext(b"arret\0".as_ptr() as *const _, cgx.llx);
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let outer_function_type = cgx.function_to_llvm_type(
            true,
            &[abitype::ABIType::Boxed(abitype::BoxedABIType::List(
                &abitype::BoxedABIType::Any,
            ))],
            &abitype::RetABIType::Inhabited(abitype::ABIType::Boxed(abitype::BoxedABIType::Any)),
        );

        let function = LLVMAddFunction(
            module,
            b"portal\0".as_ptr() as *const _,
            outer_function_type,
        );
        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let inner_function_type =
            cgx.function_to_llvm_type(rust_fun.takes_task(), rust_fun.params(), rust_fun.ret());

        let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);
        let inner_entry_point_int = LLVMConstInt(llvm_i64, rust_fun.entry_point() as u64, 0);

        let inner_entry_point = LLVMBuildIntToPtr(
            builder,
            inner_entry_point_int,
            LLVMPointerType(inner_function_type, 0),
            b"entry_point\0".as_ptr() as *const _,
        );

        let mut args = vec![];

        if rust_fun.takes_task() {
            args.push(LLVMGetParam(function, 0));
        }

        let mut arg_list = LLVMGetParam(function, 1);
        args.extend(rust_fun.params().iter().map(|_| {
            let llvm_pair = cgx
                .boxed_abi_to_llvm_type(&abitype::BoxedABIType::Pair(&abitype::BoxedABIType::Any));

            let pair_ptr = LLVMBuildBitCast(
                builder,
                arg_list,
                llvm_pair,
                b"pair_cast\0".as_ptr() as *const _,
            );

            let head_ptr =
                LLVMBuildStructGEP(builder, pair_ptr, 2, b"head_ptr\0".as_ptr() as *const _);
            let rest_ptr =
                LLVMBuildStructGEP(builder, pair_ptr, 3, b"rest_ptr\0".as_ptr() as *const _);

            let arg_value = LLVMBuildLoad(builder, head_ptr, "head\0".as_ptr() as *const _);
            arg_list = LLVMBuildLoad(builder, rest_ptr, "rest\0".as_ptr() as *const _);

            arg_value
        }));

        match rust_fun.ret() {
            abitype::RetABIType::Inhabited(abi_type) => {
                let inner_ret = LLVMBuildCall(
                    builder,
                    inner_entry_point,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    b"ret\0".as_ptr() as *const _,
                );

                let outer_ret = convert_to_boxed_any(cgx, builder, abi_type, inner_ret);
                LLVMBuildRet(builder, outer_ret);
            }
            abitype::RetABIType::Void | abitype::RetABIType::Never => {
                LLVMBuildCall(
                    builder,
                    inner_entry_point,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    b"\0".as_ptr() as *const _,
                );

                LLVMBuildUnreachable(builder);
            }
        }

        LLVMDisposeBuilder(builder);

        let error: *mut *mut c_char = ptr::null_mut();
        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            error,
        );

        //LLVMDumpModule(module);

        let mut ee = mem::uninitialized();
        let mut out = mem::zeroed();

        LLVMCreateExecutionEngineForModule(&mut ee, module, &mut out);
        let addr = LLVMGetFunctionAddress(ee, b"portal\0".as_ptr() as *const _);

        Portal {
            execution_engine: ee,
            entry_point: mem::transmute(addr),
        }
    }
}
