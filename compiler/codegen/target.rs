use std::{ffi, mem, ptr};

use libc;

use llvm_sys::core::*;
use llvm_sys::target_machine::*;

enum TripleString {
    Cross(ffi::CString),
    LLVMDefault(*mut libc::c_char),
}

impl TripleString {
    fn as_ptr(&self) -> *const libc::c_char {
        match self {
            TripleString::Cross(cross_triple) => {
                cross_triple.as_bytes_with_nul().as_ptr() as *const _
            }
            TripleString::LLVMDefault(llvm_default) => *llvm_default,
        }
    }
}

impl Drop for TripleString {
    fn drop(&mut self) {
        if let TripleString::LLVMDefault(llvm_default) = self {
            unsafe {
                LLVMDisposeMessage(*llvm_default);
            }
        }
    }
}

pub fn create_target_machine(
    cross_triple: Option<&str>,
    reloc_mode: LLVMRelocMode,
    code_model: LLVMCodeModel,
) -> LLVMTargetMachineRef {
    let cross_triple = cross_triple.map(|cross_triple| ffi::CString::new(cross_triple).unwrap());

    unsafe {
        let mut target: LLVMTargetRef = mem::uninitialized();

        let triple_string = cross_triple
            .map(|cross_triple| TripleString::Cross(ffi::CString::new(cross_triple).unwrap()))
            .unwrap_or_else(|| TripleString::LLVMDefault(LLVMGetDefaultTargetTriple()));

        let mut error: *mut libc::c_char = ptr::null_mut();
        if LLVMGetTargetFromTriple(triple_string.as_ptr(), &mut target, &mut error as *mut _) != 0 {
            panic!(
                "LLVMGetTargetFromTriple({:?}): {}",
                ffi::CStr::from_ptr(triple_string.as_ptr())
                    .to_str()
                    .unwrap(),
                ffi::CStr::from_ptr(error).to_str().unwrap()
            );
        }

        LLVMCreateTargetMachine(
            target,
            triple_string.as_ptr(),
            ptr::null(),
            ptr::null(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            reloc_mode,
            code_model,
        )
    }
}
