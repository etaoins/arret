use std::collections::HashMap;
use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use arret_runtime::boxed::RecordClassId;
use arret_runtime::intern;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::debug_info::DebugInfoBuilder;
use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;
use crate::source::SourceLoader;

pub struct ModCtx<'am, 'sl, 'interner> {
    pub module: LLVMModuleRef,

    analysed_mod: &'am AnalysedMod<'am>,
    di_builder: Option<DebugInfoBuilder<'sl>>,
    llvm_private_funs: HashMap<ops::PrivateFunId, LLVMValueRef>,

    jit_interner: Option<&'interner mut intern::Interner>,

    has_jit_record_struct_class_ids: bool,
    record_struct_class_ids: HashMap<ops::RecordStructId, RecordClassId>,
    record_structs: Vec<ops::RecordStructId>,

    record_class_id_llvm_values: Vec<LLVMValueRef>,

    function_pass_manager: LLVMPassManagerRef,
}

pub struct GeneratedMod {
    pub llvm_module: LLVMModuleRef,
    pub llvm_entry_fun: LLVMValueRef,
    pub llvm_global_interned_names: LLVMValueRef,
    pub llvm_classmap_classes: LLVMValueRef,
}

impl<'am, 'sl, 'interner> ModCtx<'am, 'sl, 'interner> {
    /// Constructs a new module context with the given name
    ///
    /// Note that the module name in LLVM is not arbitrary. For instance, in the ORC JIT it will
    /// shadow exported symbol names. This identifier should be as unique and descriptive as
    /// possible.
    fn new(
        tcx: &mut TargetCtx,
        name: &ffi::CStr,
        analysed_mod: &'am AnalysedMod<'am>,
        jit_interner: Option<&'interner mut intern::Interner>,
        jit_record_struct_class_ids: HashMap<ops::RecordStructId, RecordClassId>,
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

            has_jit_record_struct_class_ids: !jit_record_struct_class_ids.is_empty(),
            record_struct_class_ids: jit_record_struct_class_ids,
            record_structs: vec![],
            record_class_id_llvm_values: vec![],

            function_pass_manager,
        }
    }

    pub fn intern_name(&mut self, name: &str) -> intern::InternedSym {
        if let Some(ref mut jit_interner) = self.jit_interner {
            jit_interner.intern_static(name)
        } else if let Some(interned_sym) = intern::InternedSym::try_from_inline_name(name) {
            interned_sym
        } else {
            *self
                .analysed_mod
                .global_interned_names()
                .get(name)
                .expect("encountered name not found during analysis")
        }
    }

    pub fn record_class_id_for_struct(
        &mut self,
        record_struct: &ops::RecordStructId,
    ) -> RecordClassId {
        if let Some(record_class_id) = self.record_struct_class_ids.get(record_struct) {
            return *record_class_id;
        }

        let record_class_id = self.record_structs.len() as u32;
        self.record_structs.push(record_struct.clone());

        self.record_struct_class_ids
            .insert(record_struct.clone(), record_class_id);
        record_class_id
    }

    pub fn add_record_class_id_range_metadata(&mut self, record_class_id_llvm_value: LLVMValueRef) {
        // This is a bit of a hack - we don't know the range of the record class IDs until we
        // finish generating the module.
        self.record_class_id_llvm_values
            .push(record_class_id_llvm_value);
    }

    pub fn llvm_private_fun(&self, private_fun_id: ops::PrivateFunId) -> LLVMValueRef {
        self.llvm_private_funs[&private_fun_id]
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
        F: FnOnce(LLVMValueRef),
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

    fn finalise_record_class_id_range_metadata(&mut self, tcx: &mut TargetCtx) {
        unsafe {
            if self.has_jit_record_struct_class_ids {
                // These are from a distinct range; it's too much effort to include them in the JIT
                // case so just skip generating metadata.
                return;
            }

            let mut llvm_range_values = [
                LLVMValueAsMetadata(LLVMConstInt(tcx.record_class_id_llvm_type(), 0 as u64, 0)),
                LLVMValueAsMetadata(LLVMConstInt(
                    tcx.record_class_id_llvm_type(),
                    self.record_structs.len() as u64,
                    0,
                )),
            ];

            let range_md_kind_id = tcx.llvm_md_kind_id_for_name(b"range");
            let record_class_id_range_md = LLVMMDNodeInContext2(
                tcx.llx,
                llvm_range_values.as_mut_ptr(),
                llvm_range_values.len(),
            );

            for llvm_value in self.record_class_id_llvm_values.iter() {
                LLVMSetMetadata(
                    *llvm_value,
                    range_md_kind_id,
                    LLVMMetadataAsValue(tcx.llx, record_class_id_range_md),
                );
            }
        }
    }

    /// Finalise the module and return the LLVMModuleRef
    ///
    /// This will verify the module's correctness and dump the LLVM IR to stdout if the
    /// `ARRET_DUMP_LLVM` environment variable is set
    fn into_generated_mod(mut self, tcx: &mut TargetCtx) -> GeneratedMod {
        use crate::codegen::analysis::AnalysedFun;
        use crate::codegen::const_gen::gen_global_interned_names;
        use crate::codegen::fun_gen::{declare_fun, define_fun};

        // Define our entry fun
        let AnalysedFun {
            ops_fun: entry_ops_fun,
            captures: entry_captures,
        } = self.analysed_mod.entry_fun();

        let llvm_entry_fun = declare_fun(tcx, self.module, entry_ops_fun);
        define_fun(
            tcx,
            &mut self,
            entry_ops_fun,
            entry_captures,
            llvm_entry_fun,
        );

        if let Some(ref mut di_builder) = self.di_builder {
            di_builder.add_function_debug_info(
                entry_ops_fun.span,
                entry_ops_fun.source_name.as_ref(),
                llvm_entry_fun,
            );
        }

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

        let llvm_global_interned_names = gen_global_interned_names(
            tcx,
            self.module,
            self.analysed_mod.global_interned_names().keys(),
        );

        let llvm_classmap_classes =
            record_struct::gen_classmap_classes(tcx, self.module, &self.record_structs);

        self.finalise_record_class_id_range_metadata(tcx);

        if let Some(ref mut di_builder) = self.di_builder {
            di_builder.finalise();
        }

        GeneratedMod {
            llvm_module: self.module,
            llvm_entry_fun,
            llvm_global_interned_names,
            llvm_classmap_classes,
        }
    }
}

pub fn gen_mod<'am, 'sl, 'interner>(
    tcx: &mut TargetCtx,
    name: &ffi::CStr,
    analysed_mod: &'am AnalysedMod<'am>,
    jit_interner: Option<&'interner mut intern::Interner>,
    jit_record_struct_class_ids: HashMap<ops::RecordStructId, RecordClassId>,
    debug_source_loader: Option<&'sl SourceLoader>,
) -> GeneratedMod {
    ModCtx::new(
        tcx,
        name,
        analysed_mod,
        jit_interner,
        jit_record_struct_class_ids,
        debug_source_loader,
    )
    .into_generated_mod(tcx)
}

impl Drop for ModCtx<'_, '_, '_> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.function_pass_manager);
        }
    }
}
