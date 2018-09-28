use std::ffi::CStr;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::mir::ops;

pub struct ModCtx {
    pub module: LLVMModuleRef,
    pub built_funs: Vec<LLVMValueRef>,
}

impl ModCtx {
    pub fn new(module: LLVMModuleRef) -> ModCtx {
        ModCtx {
            module,
            built_funs: vec![],
        }
    }

    pub fn push_built_fun_value(
        &mut self,
        built_fun_id: ops::BuiltFunId,
        llvm_value: LLVMValueRef,
    ) {
        assert_eq!(self.built_funs.len(), built_fun_id.to_usize());
        self.built_funs.push(llvm_value);
    }

    pub fn built_fun_value(&self, built_fun_id: ops::BuiltFunId) -> LLVMValueRef {
        self.built_funs[built_fun_id.to_usize()]
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
