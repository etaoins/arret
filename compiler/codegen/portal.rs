use std::{mem, ptr};

use libc;

use llvm_sys::analysis::*;
use llvm_sys::core::*;

use runtime;
use runtime::abitype::{ABIType, BoxedABIType, RetABIType};
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::codegen::convert::convert_to_boxed_any;
use crate::codegen::jit;
use crate::codegen::CodegenCtx;
use crate::hir::rfi;

pub type Portal = fn(&mut runtime::task::Task, Gc<boxed::Any>) -> Gc<boxed::Any>;

pub fn create_portal_for_rust_fun(
    cgx: &mut CodegenCtx,
    jcx: &mut jit::JITCtx,
    rust_fun: &rfi::Fun,
) -> Portal {
    use runtime::boxed::TypeTag;

    unsafe {
        // Create the module
        let module = LLVMModuleCreateWithNameInContext(b"arret\0".as_ptr() as *const _, cgx.llx);
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let outer_function_type = cgx.function_to_llvm_type(
            true,
            &[ABIType::Boxed(BoxedABIType::List(&BoxedABIType::Any))],
            &RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Any)),
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
        let mut param_type_iter = rust_fun.params().iter();

        let rest_type = if rust_fun.arret_fun_type().params().rest().is_some() {
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

        match rust_fun.ret() {
            RetABIType::Inhabited(abi_type) => {
                let inner_ret = LLVMBuildCall(
                    builder,
                    inner_entry_point,
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
                    inner_entry_point,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    b"\0".as_ptr() as *const _,
                );

                if rust_fun.ret() == &RetABIType::Never {
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

        let error: *mut *mut libc::c_char = ptr::null_mut();
        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            error,
        );

        //LLVMDumpModule(module);

        let address = jcx.compile_fun(module, b"portal\0");
        mem::transmute(address)
    }
}
