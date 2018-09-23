use std::{ffi, mem, ptr};

use libc;

use llvm_sys::target::*;
use llvm_sys::target_machine::*;

pub fn create_target_machine(
    cross_triple: Option<&str>,
    reloc_mode: LLVMRelocMode,
    code_model: LLVMCodeModel,
) -> LLVMTargetMachineRef {
    let cross_triple = cross_triple.map(|cross_triple| ffi::CString::new(cross_triple).unwrap());

    unsafe {
        let mut target: LLVMTargetRef = mem::uninitialized();

        if cross_triple.is_some() {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
        } else {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
        }

        let triple = cross_triple
            .as_ref()
            .map(|cross_triple| cross_triple.as_ptr() as *const libc::c_char)
            .unwrap_or_else(|| LLVMGetDefaultTargetTriple());

        let mut error: *mut libc::c_char = ptr::null_mut();
        if LLVMGetTargetFromTriple(triple, &mut target, &mut error as *mut _) != 0 {
            panic!(
                "LLVMGetTargetFromTriple({:?}): {}",
                ffi::CStr::from_ptr(triple).to_str().unwrap(),
                ffi::CStr::from_ptr(error).to_str().unwrap()
            );
        }

        LLVMCreateTargetMachine(
            target,
            triple,
            ptr::null(),
            ptr::null(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            reloc_mode,
            code_model,
        )
    }
}
