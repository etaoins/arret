use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::codegen::fun_gen::BuiltFun;
use crate::mir::ops;

pub struct ModCtx {
    pub module: LLVMModuleRef,
    pub function_pass_manager: LLVMPassManagerRef,
    pub built_funs: Vec<BuiltFun>,
}

impl Drop for ModCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.function_pass_manager);
        }
    }
}

impl ModCtx {
    pub fn new(module: LLVMModuleRef) -> ModCtx {
        use llvm_sys::transforms::pass_manager_builder::*;

        unsafe {
            let function_pass_manager = LLVMCreateFunctionPassManagerForModule(module);

            let fpmb = LLVMPassManagerBuilderCreate();
            LLVMPassManagerBuilderSetOptLevel(fpmb, 2);
            LLVMPassManagerBuilderPopulateFunctionPassManager(fpmb, function_pass_manager);
            LLVMPassManagerBuilderDispose(fpmb);

            ModCtx {
                module,
                function_pass_manager,
                built_funs: vec![],
            }
        }
    }

    pub fn push_built_fun(&mut self, built_fun_id: ops::BuiltFunId, built_fun: BuiltFun) {
        assert_eq!(self.built_funs.len(), built_fun_id.to_usize());
        self.built_funs.push(built_fun);
    }

    pub fn built_funs(&self) -> &Vec<BuiltFun> {
        &self.built_funs
    }

    pub fn get_global_or_insert<F>(
        &mut self,
        llvm_type: LLVMTypeRef,
        name: &[u8],
        initial_value: F,
    ) -> LLVMValueRef
    where
        F: FnOnce() -> LLVMValueRef,
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

    pub fn get_function_or_insert<F>(
        &mut self,
        function_type: LLVMTypeRef,
        name: &[u8],
        initialise: F,
    ) -> LLVMValueRef
    where
        F: FnOnce(LLVMValueRef) -> (),
    {
        unsafe {
            let function = LLVMGetNamedFunction(self.module, name.as_ptr() as *const _);

            if !function.is_null() {
                return function;
            }

            let function = LLVMAddFunction(self.module, name.as_ptr() as *const _, function_type);

            initialise(function);
            function
        }
    }

    pub fn optimise_function(&mut self, function: LLVMValueRef) {
        unsafe {
            LLVMRunFunctionPassManager(self.function_pass_manager, function);
        }
    }
}
