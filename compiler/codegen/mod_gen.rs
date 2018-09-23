use std::ffi::CStr;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

pub struct ModCtx {
    pub module: LLVMModuleRef,
}

impl ModCtx {
    pub fn new(module: LLVMModuleRef) -> ModCtx {
        ModCtx { module }
    }

    pub fn get_global_or_insert<F>(
        &mut self,
        llvm_type: LLVMTypeRef,
        name: &CStr,
        mut initial_value: F,
    ) -> LLVMValueRef
    where
        F: FnMut() -> LLVMValueRef,
    {
        unsafe {
            let global = LLVMGetNamedGlobal(self.module, name.as_ptr() as *const _);

            if !global.is_null() {
                return global;
            }

            let global = LLVMAddGlobal(self.module, llvm_type, name.as_ptr() as *const _);
            LLVMSetInitializer(global, initial_value());

            global
        }
    }
}
