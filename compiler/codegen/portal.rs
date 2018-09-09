use std::{ffi, mem, ptr};

use libc;

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime;
use runtime::abitype::{ABIType, BoxedABIType, RetABIType};
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::codegen::convert::convert_to_boxed_any;
use crate::codegen::fun_abi::FunABI;
use crate::codegen::jit;
use crate::codegen::CodegenCtx;
use crate::hir::rfi;

pub type Portal = extern "C" fn(&mut runtime::task::Task, Gc<boxed::Any>) -> Gc<boxed::Any>;

/// Compiles a portal to the passed `rust_fun` in the given JIT
pub fn jit_portal_for_rust_fun(
    cgx: &mut CodegenCtx,
    jcx: &mut jit::JITCtx,
    rust_fun: &rfi::Fun,
) -> Portal {
    unsafe {
        // Create some names
        let inner_symbol = ffi::CString::new(rust_fun.symbol()).unwrap();
        let outer_symbol = ffi::CString::new(format!("{}_portal", rust_fun.symbol())).unwrap();

        // Add the inner symbol
        jcx.add_symbol(
            inner_symbol.as_bytes_with_nul(),
            rust_fun.entry_point() as u64,
        );

        // Create the module
        let module = LLVMModuleCreateWithNameInContext(
            outer_symbol.as_bytes_with_nul().as_ptr() as *const _,
            cgx.llx,
        );

        // Create the inner function value
        let inner_function_type =
            cgx.function_to_llvm_type(rust_fun.takes_task(), rust_fun.params(), rust_fun.ret());

        let inner_function = LLVMAddGlobal(
            module,
            inner_function_type,
            inner_symbol.as_bytes_with_nul().as_ptr() as *const _,
        );

        // Build the inner portal
        build_portal_for_fun(cgx, module, &outer_symbol, rust_fun, inner_function);

        let error: *mut *mut libc::c_char = ptr::null_mut();
        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            error,
        );

        //LLVMDumpModule(module);

        let address = jcx.compile_fun(module, outer_symbol.as_bytes_with_nul());
        mem::transmute(address)
    }
}

pub fn build_portal_for_fun(
    cgx: &mut CodegenCtx,
    module: LLVMModuleRef,
    outer_symbol: &ffi::CString,
    fun_abi: &impl FunABI,
    fun_value: LLVMValueRef,
) {
    use runtime::boxed::TypeTag;

    unsafe {
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        // Create the outer function
        let outer_function_type = cgx.function_to_llvm_type(
            true,
            &[ABIType::Boxed(BoxedABIType::List(&BoxedABIType::Any))],
            &RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Any)),
        );

        let function = LLVMAddFunction(
            module,
            outer_symbol.as_bytes_with_nul().as_ptr() as *const _,
            outer_function_type,
        );
        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);
        let mut args = vec![];

        if fun_abi.takes_task() {
            args.push(LLVMGetParam(function, 0));
        }

        let mut arg_list = LLVMGetParam(function, 1);
        let mut param_type_iter = fun_abi.params().iter();

        let rest_type = if fun_abi.has_rest() {
            param_type_iter.next_back()
        } else {
            None
        };

        args.extend(param_type_iter.map(|_| {
            let llvm_pair = cgx.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Pair(&BoxedABIType::Any));

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

        if rest_type.is_some() {
            args.push(arg_list);
        }

        match fun_abi.ret() {
            RetABIType::Inhabited(abi_type) => {
                let inner_ret = LLVMBuildCall(
                    builder,
                    fun_value,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    b"ret\0".as_ptr() as *const _,
                );

                let outer_ret = convert_to_boxed_any(cgx, module, builder, abi_type, inner_ret);
                LLVMBuildRet(builder, outer_ret);
            }
            RetABIType::Void | RetABIType::Never => {
                LLVMBuildCall(
                    builder,
                    fun_value,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    b"\0".as_ptr() as *const _,
                );

                if fun_abi.ret() == &RetABIType::Never {
                    LLVMBuildUnreachable(builder);
                } else {
                    let llvm_any_ptr = cgx.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);

                    let nil_ret = LLVMConstBitCast(
                        cgx.ptr_to_singleton_box(
                            module,
                            &BoxedABIType::DirectTagged(TypeTag::Nil),
                            b"ARRET_NIL\0",
                        ),
                        llvm_any_ptr,
                    );
                    LLVMBuildRet(builder, nil_ret);
                }
            }
        }

        LLVMDisposeBuilder(builder);
    }
}
