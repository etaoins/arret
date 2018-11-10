use std::collections::HashMap;
use std::{env, ffi, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::fun_gen::GenedFun;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;

pub struct ModCtx<'am> {
    pub module: LLVMModuleRef,

    analysed_mod: &'am AnalysedMod<'am>,
    gened_private_funs: HashMap<ops::PrivateFunId, GenedFun>,

    function_pass_manager: LLVMPassManagerRef,
}

impl<'pf> Drop for ModCtx<'pf> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.function_pass_manager);
        }
    }
}

impl<'am> ModCtx<'am> {
    /// Constructs a new module context with the given name
    ///
    /// Note that the module name in LLVM is not arbitrary. For instance, in the ORC JIT it will
    /// shadow exported symbol names. This identifier should be as unique and descriptive as
    /// possible.
    pub fn new(
        tcx: &TargetCtx,
        name: &ffi::CStr,
        analysed_mod: &'am AnalysedMod<'am>,
    ) -> ModCtx<'am> {
        use llvm_sys::transforms::pass_manager_builder::*;

        unsafe {
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr() as *const _, tcx.llx);
            LLVMSetModuleDataLayout(module, tcx.target_data());

            let target_triple = LLVMGetTargetMachineTriple(tcx.target_machine());
            LLVMSetTarget(module, target_triple);
            LLVMDisposeMessage(target_triple);

            let function_pass_manager = LLVMCreateFunctionPassManagerForModule(module);

            if tcx.optimising() {
                let fpmb = LLVMPassManagerBuilderCreate();
                LLVMPassManagerBuilderSetOptLevel(fpmb, 2);
                LLVMPassManagerBuilderPopulateFunctionPassManager(fpmb, function_pass_manager);
                LLVMPassManagerBuilderDispose(fpmb);
            }

            ModCtx {
                module,

                analysed_mod,
                gened_private_funs: HashMap::new(),

                function_pass_manager,
            }
        }
    }

    pub fn gened_private_fun(
        &mut self,
        tcx: &mut TargetCtx,
        private_fun_id: ops::PrivateFunId,
    ) -> &GenedFun {
        use crate::codegen::fun_gen::gen_fun;

        // TODO: This is a hack around lifetimes
        if self.gened_private_funs.contains_key(&private_fun_id) {
            return &self.gened_private_funs[&private_fun_id];
        }

        let ops_fun = self.analysed_mod.private_fun(private_fun_id);
        let captures = self.analysed_mod.private_fun_captures(private_fun_id);
        let gened_fun = gen_fun(tcx, self, ops_fun, captures);

        unsafe {
            LLVMSetLinkage(gened_fun.llvm_value, LLVMLinkage::LLVMPrivateLinkage);
        }

        self.gened_private_funs
            .entry(private_fun_id)
            .or_insert(gened_fun)
    }

    pub fn gened_entry_fun(&mut self, tcx: &mut TargetCtx) -> GenedFun {
        crate::codegen::fun_gen::gen_fun(
            tcx,
            self,
            self.analysed_mod.entry_fun(),
            self.analysed_mod.entry_fun_captures(),
        )
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

    /// Finalise the module and return the LLVMModuleRef
    ///
    /// This will verify the module's correctness and dump the LLVM IR to stdout if the
    /// `ARRET_DUMP_LLVM` environment variable is set
    pub fn into_llvm_module(self) -> LLVMModuleRef {
        unsafe {
            let mut error: *mut libc::c_char = ptr::null_mut();

            if env::var_os("ARRET_DUMP_LLVM").is_some() {
                LLVMDumpModule(self.module);
            }

            LLVMVerifyModule(
                self.module,
                LLVMVerifierFailureAction::LLVMAbortProcessAction,
                &mut error as *mut _,
            );
            LLVMDisposeMessage(error);
        }
        self.module
    }
}
