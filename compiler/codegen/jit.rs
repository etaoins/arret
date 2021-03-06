use std::collections::HashMap;
use std::{alloc, env, ffi, io, ptr};

use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::orc::*;
use llvm_sys::target_machine::*;

use arret_runtime::boxed;
use arret_runtime::class_map::ClassMap;
use arret_runtime::intern::Interner;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::mod_gen::{gen_mod, GeneratedMod};
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;
use crate::mir::printer::print_fun;

extern "C" fn orc_sym_resolve(name_ptr: *const libc::c_char, jcx_void: *mut libc::c_void) -> u64 {
    unsafe {
        let jcx: &JitCtx = &*(jcx_void as *mut _);
        let name = ffi::CStr::from_ptr(name_ptr);
        jcx.symbols
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("unable to lookup symbol {:?}", name))
    }
}

pub struct JitCtx {
    tcx: TargetCtx,
    orc: LLVMOrcJITStackRef,
    target_machine: LLVMTargetMachineRef,
    symbols: HashMap<ffi::CString, u64>,
    record_struct_class_ids: HashMap<ops::RecordStructId, boxed::RecordClassId>,

    module_counter: usize,
}

pub struct RegisteredRecordStruct {
    /// Allocation layout of the record's data
    pub data_layout: Option<alloc::Layout>,
    /// Record class ID that was dynamically registered in the class map
    pub record_class_id: boxed::RecordClassId,
}

impl JitCtx {
    pub fn new(optimising: bool) -> JitCtx {
        #[allow(clippy::fn_to_numeric_cast)]
        unsafe {
            use crate::codegen::target_machine::create_target_machine;
            use arret_runtime::compiler_support;

            LLVMLinkInMCJIT();

            let target_machine = create_target_machine(
                None, // Can't cross compile in the JIT
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelJITDefault,
            );
            let orc = LLVMOrcCreateInstance(target_machine);

            let mut jcx = JitCtx {
                tcx: TargetCtx::new(target_machine, optimising),
                orc,
                target_machine,
                symbols: HashMap::new(),
                record_struct_class_ids: HashMap::new(),

                module_counter: 0,
            };

            jcx.add_symbol(b"ARRET_TRUE\0", &boxed::TRUE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_FALSE\0", &boxed::FALSE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_NIL\0", &boxed::NIL_INSTANCE as *const _ as u64);
            jcx.add_symbol(
                b"arret_runtime_alloc_cells\0",
                compiler_support::alloc_cells as u64,
            );
            jcx.add_symbol(
                b"arret_runtime_alloc_record_data\0",
                compiler_support::alloc_record_data as u64,
            );
            jcx.add_symbol(b"arret_runtime_equals\0", compiler_support::equals as u64);
            jcx.add_symbol(
                b"arret_runtime_panic_with_string\0",
                compiler_support::panic_with_string as u64,
            );

            jcx
        }
    }

    pub fn compile_fun(
        &mut self,
        private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
        interner: &mut Interner,
        fun: &ops::Fun,
    ) -> u64 {
        if env::var_os("ARRET_DUMP_MIR").is_some() {
            print_fun(&mut io::stdout().lock(), private_funs, fun, None).unwrap();
        }

        let tcx = &mut self.tcx;

        self.module_counter += 1;

        let module_counter = self.module_counter;
        let module_name = fun
            .source_name
            .as_ref()
            .map(|source_name| format!("JIT Module #{} for `{}`\0", module_counter, source_name))
            .unwrap_or_else(|| format!("Anonymous JIT Module #{}\0", module_counter));

        // Create the module
        let analysed_mod = AnalysedMod::new(private_funs, fun);

        unsafe {
            // Generate our Arret funs
            let GeneratedMod {
                llvm_module,
                llvm_entry_fun,
                ..
            } = gen_mod(
                tcx,
                module_name.as_bytes(),
                &analysed_mod,
                Some(interner),
                self.record_struct_class_ids.clone(),
                None,
            );

            // We need to take ownership before we transfer the module to ORC
            let mut function_name_len: usize = 0;
            let function_name_ptr = LLVMGetValueName2(llvm_entry_fun, &mut function_name_len);
            let function_name = ffi::CStr::from_ptr(function_name_ptr).to_owned();

            tcx.finish_module(llvm_module);

            let mut orc_module: LLVMOrcModuleHandle = 0;
            if !LLVMOrcAddEagerlyCompiledIR(
                self.orc,
                &mut orc_module,
                llvm_module,
                Some(orc_sym_resolve),
                self as *mut JitCtx as *mut _,
            )
            .is_null()
            {
                panic!("Unable to add module");
            }

            let mut target_address: LLVMOrcTargetAddress = 0;
            if !LLVMOrcGetSymbolAddressIn(
                self.orc,
                &mut target_address,
                orc_module,
                function_name.as_ptr() as *const _,
            )
            .is_null()
            {
                panic!("Unable to get symbol address")
            }

            target_address
        }
    }

    pub fn add_symbol(&mut self, unmangled_name: &[u8], address: u64) {
        unsafe {
            let mut mangled_pointer: *mut libc::c_char = ptr::null_mut();
            LLVMOrcGetMangledSymbol(
                self.orc,
                &mut mangled_pointer,
                unmangled_name.as_ptr() as *const _,
            );

            let mangled_string = ffi::CStr::from_ptr(mangled_pointer);
            self.symbols.insert(mangled_string.to_owned(), address);

            LLVMOrcDisposeMangledSymbol(mangled_pointer);
        }
    }

    pub fn register_record_struct(
        &mut self,
        record_struct: &ops::RecordStructId,
        class_map: &mut ClassMap,
    ) -> RegisteredRecordStruct {
        let target_record_struct = self.tcx.target_record_struct(record_struct);
        let record_class_id =
            class_map.push_dynamic_class(target_record_struct.classmap_class.clone());

        self.record_struct_class_ids
            .insert(record_struct.clone(), record_class_id);

        RegisteredRecordStruct {
            data_layout: target_record_struct.data_layout,
            record_class_id,
        }
    }
}

impl Drop for JitCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.target_machine);
            LLVMOrcDisposeInstance(self.orc);
        }
    }
}
