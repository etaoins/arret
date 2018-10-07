use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMAttributeFunctionIndex;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops;

pub fn gen_static_symbol_callee(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    static_symbol: &ops::StaticSymbol,
) -> LLVMValueRef {
    use crate::codegen::escape_analysis::{infer_param_capture_kind, CaptureKind};
    use runtime::abitype::{ABIType, RetABIType};

    let ops::StaticSymbol { abi, symbol } = static_symbol;

    let function_type = cgx.fun_abi_to_llvm_type(&abi);
    let function_name = ffi::CString::new(*symbol).unwrap();

    unsafe {
        let global = LLVMGetNamedFunction(
            mcx.module,
            function_name.as_bytes_with_nul().as_ptr() as *const _,
        );

        if !global.is_null() {
            return global;
        }

        let function = LLVMAddFunction(
            mcx.module,
            function_name.as_bytes_with_nul().as_ptr() as *const _,
            function_type,
        );

        // LLVM param attributes are 1 indexed
        let param_attr_offset = 1 + (abi.takes_task as usize) + (abi.takes_closure as usize);

        for (index, param_abi_type) in abi.params.iter().enumerate() {
            if let ABIType::Boxed(_) = param_abi_type {
                let no_capture =
                    infer_param_capture_kind(&abi.ret, &param_abi_type) == CaptureKind::Never;

                cgx.add_boxed_param_attrs(function, (param_attr_offset + index) as u32, no_capture)
            }
        }

        match abi.ret {
            RetABIType::Inhabited(ABIType::Boxed(_)) => {
                cgx.add_boxed_return_attrs(function);
            }
            RetABIType::Never => {
                let noreturn_attr = cgx.llvm_enum_attr_for_name(b"noreturn", 0);
                LLVMAddAttributeAtIndex(function, LLVMAttributeFunctionIndex, noreturn_attr);
            }
            _ => {}
        }

        function
    }
}

pub fn gen_boxed_fun_thunk_callee(
    builder: LLVMBuilderRef,
    llvm_fun_thunk: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let entry_ptr = LLVMBuildStructGEP(
            builder,
            llvm_fun_thunk,
            2,
            b"fun_thunk_entry_ptr\0".as_ptr() as *const _,
        );

        LLVMBuildLoad(builder, entry_ptr, "fun_thunk_entry\0".as_ptr() as *const _)
    }
}

pub fn gen_built_fun_callee(mcx: &mut ModCtx, built_fun_id: ops::BuiltFunId) -> LLVMValueRef {
    mcx.built_fun(built_fun_id).llvm_value
}
