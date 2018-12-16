use std::collections::HashMap;
use std::{env, ffi, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::debug_info::DebugInfoBuilder;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;
use crate::source::SourceLoader;

pub struct ModCtx<'am, 'sl> {
    pub module: LLVMModuleRef,

    analysed_mod: &'am AnalysedMod<'am>,
    di_builder: Option<DebugInfoBuilder<'sl>>,
    llvm_private_funs: HashMap<ops::PrivateFunId, LLVMValueRef>,

    function_pass_manager: LLVMPassManagerRef,
}

impl<'am, 'sl> ModCtx<'am, 'sl> {
    /// Constructs a new module context with the given name
    ///
    /// Note that the module name in LLVM is not arbitrary. For instance, in the ORC JIT it will
    /// shadow exported symbol names. This identifier should be as unique and descriptive as
    /// possible.
    pub fn new(
        tcx: &TargetCtx,
        name: &ffi::CStr,
        analysed_mod: &'am AnalysedMod<'am>,
        debug_source_loader: Option<&'sl SourceLoader>,
    ) -> ModCtx<'am, 'sl> {
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

            let di_builder = debug_source_loader.map(|source_loader| {
                DebugInfoBuilder::new(
                    source_loader,
                    tcx.optimising(),
                    analysed_mod.entry_fun().span,
                    module,
                )
            });

            ModCtx {
                module,

                analysed_mod,
                di_builder,
                llvm_private_funs: HashMap::new(),

                function_pass_manager,
            }
        }
    }

    pub fn llvm_private_fun(
        &mut self,
        tcx: &mut TargetCtx,
        private_fun_id: ops::PrivateFunId,
    ) -> LLVMValueRef {
        use crate::codegen::fun_gen::{declare_fun, define_fun};

        // TODO: This is a hack around lifetimes
        if self.llvm_private_funs.contains_key(&private_fun_id) {
            return self.llvm_private_funs[&private_fun_id];
        }

        let ops_fun = self.analysed_mod.private_fun(private_fun_id);
        let captures = self.analysed_mod.private_fun_captures(private_fun_id);

        let llvm_fun = declare_fun(tcx, self.module, ops_fun);
        define_fun(tcx, self, ops_fun, captures, llvm_fun);

        if let Some(ref mut di_builder) = self.di_builder {
            di_builder.add_function_debug_info(
                ops_fun.span,
                ops_fun.source_name.as_ref(),
                llvm_fun,
            );
        }

        unsafe {
            LLVMSetLinkage(llvm_fun, LLVMLinkage::LLVMPrivateLinkage);
        }

        self.llvm_private_funs.insert(private_fun_id, llvm_fun);
        llvm_fun
    }

    pub fn llvm_entry_fun(&mut self, tcx: &mut TargetCtx) -> LLVMValueRef {
        use crate::codegen::fun_gen::{declare_fun, define_fun};

        let ops_fun = self.analysed_mod.entry_fun();
        let captures = self.analysed_mod.entry_fun_captures();

        let llvm_fun = declare_fun(tcx, self.module, ops_fun);
        define_fun(tcx, self, ops_fun, captures, llvm_fun);

        if let Some(ref mut di_builder) = self.di_builder {
            di_builder.add_function_debug_info(
                ops_fun.span,
                ops_fun.source_name.as_ref(),
                llvm_fun,
            );
        }

        llvm_fun
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

    pub fn di_builder(&mut self) -> Option<&mut DebugInfoBuilder<'sl>> {
        self.di_builder.as_mut()
    }

    /// Finalise the module and return the LLVMModuleRef
    ///
    /// This will verify the module's correctness and dump the LLVM IR to stdout if the
    /// `ARRET_DUMP_LLVM` environment variable is set
    pub fn into_llvm_module(mut self) -> LLVMModuleRef {
        if let Some(ref mut di_builder) = self.di_builder {
            di_builder.finalise();
        }

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

impl Drop for ModCtx<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.function_pass_manager);
        }
    }
}
