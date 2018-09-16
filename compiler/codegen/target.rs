use std::{mem, ptr};

use llvm_sys::target::*;
use llvm_sys::target_machine::*;

pub fn default_target_machine(
    reloc_mode: LLVMRelocMode,
    code_model: LLVMCodeModel,
) -> LLVMTargetMachineRef {
    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();

        let default_triple = LLVMGetDefaultTargetTriple();

        let mut target: LLVMTargetRef = mem::uninitialized();
        if LLVMGetTargetFromTriple(default_triple, &mut target, ptr::null_mut()) != 0 {
            panic!("Can't get target triple");
        }

        LLVMCreateTargetMachine(
            target,
            default_triple,
            ptr::null(),
            ptr::null(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            reloc_mode,
            code_model,
        )
    }
}
