use std::collections::HashMap;
use std::{env, ffi, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use runtime::intern::Interner;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::debug_info::DebugInfoBuilder;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;
use crate::source::SourceLoader;

pub struct ModCtx<'am, 'sl, 'interner> {
    pub module: LLVMModuleRef,

    analysed_mod: &'am AnalysedMod<'am>,
    di_builder: Option<DebugInfoBuilder<'sl>>,
    llvm_private_funs: HashMap<ops::PrivateFunId, LLVMValueRef>,
    jit_interner: Option<&'interner mut Interner>,

    function_pass_manager: LLVMPassManagerRef,
}

impl<'am, 'sl, 'interner> ModCtx<'am, 'sl, 'interner> {
    /// Constructs a new module context with the given name
    ///
    /// Note that the module name in LLVM is not arbitrary. For instance, in the ORC JIT it will
    /// shadow exported symbol names. This identifier should be as unique and descriptive as
    /// possible.
    pub fn new(
        tcx: &mut TargetCtx,
        name: &ffi::CStr,
        analysed_mod: &'am AnalysedMod<'am>,
        jit_interner: Option<&'interner mut Interner>,
        debug_source_loader: Option<&'sl SourceLoader>,
    ) -> Self {
        use crate::codegen::fun_gen::declare_fun;
        use llvm_sys::transforms::pass_manager_builder::*;

        // Hoist these out of the unsafe block
        let module;
        let function_pass_manager;
        unsafe {
            module = LLVMModuleCreateWithNameInContext(name.as_ptr() as *const _, tcx.llx);
            LLVMSetModuleDataLayout(module, tcx.target_data());

            let target_triple = LLVMGetTargetMachineTriple(tcx.target_machine());
            LLVMSetTarget(module, target_triple);
            LLVMDisposeMessage(target_triple);

            function_pass_manager = LLVMCreateFunctionPassManagerForModule(module);

            if tcx.optimising() {
                let fpmb = LLVMPassManagerBuilderCreate();
                LLVMPassManagerBuilderSetOptLevel(fpmb, 2);
                LLVMPassManagerBuilderPopulateFunctionPassManager(fpmb, function_pass_manager);
                LLVMPassManagerBuilderDispose(fpmb);
            }
        }

        let di_builder = debug_source_loader.map(|source_loader| {
            DebugInfoBuilder::new(
                source_loader,
                tcx.optimising(),
                analysed_mod.entry_fun().ops_fun.span,
                module,
            )
        });

        // Forward declare all our private funs
        // Analysis has determined all of these are used
        let llvm_private_funs = analysed_mod
            .private_funs()
            .map(|(private_fun_id, analysed_fun)| {
                let llvm_fun = declare_fun(tcx, module, analysed_fun.ops_fun);
                (*private_fun_id, llvm_fun)
            })
            .collect();

        ModCtx {
            module,

            analysed_mod,
            di_builder,
            llvm_private_funs,
            jit_interner,

            function_pass_manager,
        }
    }

    pub fn jit_interner(&mut self) -> &mut Option<&'interner mut Interner> {
        &mut self.jit_interner
    }

    pub fn llvm_private_fun(&self, private_fun_id: ops::PrivateFunId) -> LLVMValueRef {
        self.llvm_private_funs[&private_fun_id]
    }

    pub fn llvm_entry_fun(&mut self, tcx: &mut TargetCtx) -> LLVMValueRef {
        use crate::codegen::analysis::AnalysedFun;
        use crate::codegen::fun_gen::{declare_fun, define_fun};

        let AnalysedFun { ops_fun, captures } = self.analysed_mod.entry_fun();

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
    pub fn into_llvm_module(mut self, tcx: &mut TargetCtx) -> LLVMModuleRef {
        use crate::codegen::analysis::AnalysedFun;
        use crate::codegen::fun_gen::define_fun;

        // Define all of our private funs
        for (private_fun_id, analysed_fun) in self.analysed_mod.private_funs() {
            let AnalysedFun { ops_fun, captures } = analysed_fun;
            let llvm_fun = self.llvm_private_funs[private_fun_id];

            define_fun(tcx, &mut self, ops_fun, captures, llvm_fun);

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
        }

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

impl Drop for ModCtx<'_, '_, '_> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.function_pass_manager);
        }
    }
}
