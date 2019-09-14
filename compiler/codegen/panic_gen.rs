use std::ffi;

use llvm_sys::core::*;
use llvm_sys::LLVMAttributeFunctionIndex;

use crate::codegen::const_gen::annotate_private_global;
use crate::codegen::fun_gen::FunCtx;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;

pub(crate) fn gen_panic(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    message: &str,
) {
    unsafe {
        let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
        let llvm_i32 = LLVMInt64TypeInContext(tcx.llx);

        let llvm_param_types = &mut [LLVMPointerType(llvm_i8, 0), llvm_i32];

        let panic_with_string_llvm_type = LLVMFunctionType(
            LLVMVoidTypeInContext(tcx.llx),
            llvm_param_types.as_mut_ptr(),
            llvm_param_types.len() as u32,
            0,
        );

        let panic_with_string_fun = mcx.get_function_or_insert(
            panic_with_string_llvm_type,
            b"arret_runtime_panic_with_string\0",
            |panic_with_string_fun| {
                LLVMAddAttributeAtIndex(
                    panic_with_string_fun,
                    LLVMAttributeFunctionIndex,
                    tcx.llvm_enum_attr_for_name(b"cold", 0),
                );

                LLVMAddAttributeAtIndex(
                    panic_with_string_fun,
                    LLVMAttributeFunctionIndex,
                    tcx.llvm_enum_attr_for_name(b"noreturn", 0),
                );
            },
        );

        let llvm_message_string =
            LLVMConstStringInContext(tcx.llx, message.as_ptr() as *mut _, message.len() as u32, 1);

        let message_global_name = ffi::CString::new("panic_message").unwrap();

        let llvm_message_global = LLVMAddGlobal(
            mcx.module,
            LLVMTypeOf(llvm_message_string),
            message_global_name.as_ptr() as *const _,
        );
        LLVMSetInitializer(llvm_message_global, llvm_message_string);
        annotate_private_global(llvm_message_global);

        let llvm_first_byte_gep_indices =
            &mut [LLVMConstInt(llvm_i32, 0, 0), LLVMConstInt(llvm_i32, 0, 0)];

        let message_pointer = LLVMConstInBoundsGEP(
            llvm_message_global,
            llvm_first_byte_gep_indices.as_mut_ptr(),
            llvm_first_byte_gep_indices.len() as u32,
        );

        let panic_with_string_args = &mut [
            message_pointer,
            LLVMConstInt(llvm_i32, message.len() as u64, 0),
        ];

        LLVMBuildCall(
            fcx.builder,
            panic_with_string_fun,
            panic_with_string_args.as_mut_ptr(),
            panic_with_string_args.len() as u32,
            b"\0".as_ptr() as *const _,
        );

        LLVMBuildUnreachable(fcx.builder);
    }
}
