use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMAttributeFunctionIndex, LLVMCallConv};

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::libcstr;
use crate::mir::ops;

pub fn gen_static_symbol_entry_point(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    static_symbol: &ops::StaticSymbol,
) -> LLVMValueRef {
    use crate::codegen::analysis::escape::{infer_param_capture_kind, CaptureKind};
    use arret_runtime::abitype::{AbiType, RetAbiType};

    let ops::StaticSymbol {
        abi,
        impure,
        symbol,
    } = static_symbol;

    let function_type = tcx.fun_abi_to_llvm_type(abi);
    let function_name = ffi::CString::new(*symbol).unwrap();

    unsafe {
        mcx.get_function_or_insert(
            function_type,
            function_name.as_bytes_with_nul(),
            |function| {
                let param_attr_offset = abi.takes_task as usize;

                for (index, param_abi_type) in abi.params.iter().enumerate() {
                    if let AbiType::Boxed(_) = param_abi_type.abi_type {
                        let no_capture = infer_param_capture_kind(&abi.ret, param_abi_type)
                            == CaptureKind::Never;

                        tcx.add_boxed_param_attrs(
                            function,
                            (param_attr_offset + index) as u32,
                            no_capture,
                        )
                    }
                }

                if !impure {
                    let speculatable_attr = tcx.llvm_enum_attr_for_name("speculatable", 0);
                    LLVMAddAttributeAtIndex(
                        function,
                        LLVMAttributeFunctionIndex,
                        speculatable_attr,
                    );
                }

                match abi.ret {
                    RetAbiType::Inhabited(AbiType::Boxed(_)) => {
                        tcx.add_boxed_return_attrs(function);
                    }
                    RetAbiType::Never => {
                        let noreturn_attr = tcx.llvm_enum_attr_for_name("noreturn", 0);
                        LLVMAddAttributeAtIndex(
                            function,
                            LLVMAttributeFunctionIndex,
                            noreturn_attr,
                        );
                    }
                    _ => {}
                }
            },
        )
    }
}

pub fn gen_boxed_fun_thunk_entry_point(
    builder: LLVMBuilderRef,
    llvm_fun_thunk: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let entry_ptr =
            LLVMBuildStructGEP(builder, llvm_fun_thunk, 2, libcstr!("fun_thunk_entry_ptr"));

        LLVMBuildLoad(builder, entry_ptr, libcstr!("fun_thunk_entry"))
    }
}

pub fn callee_takes_task(callee: &ops::Callee) -> bool {
    match callee {
        ops::Callee::BoxedFunThunk(_) => true,
        ops::Callee::PrivateFun(_) => true,
        ops::Callee::StaticSymbol(ops::StaticSymbol { abi, .. }) => abi.takes_task,
    }
}

pub fn callee_call_conv(mcx: &mut ModCtx<'_, '_, '_>, callee: &ops::Callee) -> u32 {
    match callee {
        ops::Callee::BoxedFunThunk(_) | ops::Callee::StaticSymbol(_) => {
            LLVMCallConv::LLVMCCallConv as u32
        }
        ops::Callee::PrivateFun(private_fun_id) => unsafe {
            LLVMGetFunctionCallConv(mcx.llvm_private_fun(*private_fun_id))
        },
    }
}
