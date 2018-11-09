use std::collections::HashMap;
use std::{ffi, mem};

use runtime::boxed;

use libc;

use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::orc::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::mir::ops;

extern "C" fn orc_sym_resolve(name_ptr: *const libc::c_char, jcx_void: *mut libc::c_void) -> u64 {
    unsafe {
        let jcx: &JITCtx = &*(jcx_void as *mut _);
        let name = ffi::CStr::from_ptr(name_ptr);
        jcx.symbols
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("unable to lookup symbol {:?}", name))
    }
}

pub struct JITCtx {
    tcx: TargetCtx,
    orc: LLVMOrcJITStackRef,
    target_machine: LLVMTargetMachineRef,
    symbols: HashMap<ffi::CString, u64>,

    module_counter: usize,
}

impl JITCtx {
    pub fn new(optimising: bool) -> JITCtx {
        unsafe {
            use crate::codegen::target_machine::create_target_machine;
            use runtime::compiler_support;

            LLVMLinkInMCJIT();

            let target_machine = create_target_machine(
                None, // Can't cross compile in the JIT
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelJITDefault,
            );
            let orc = LLVMOrcCreateInstance(target_machine);

            let mut jcx = JITCtx {
                tcx: TargetCtx::new(target_machine, optimising),
                orc,
                target_machine,
                symbols: HashMap::new(),

                module_counter: 0,
            };

            jcx.add_symbol(b"ARRET_TRUE\0", &boxed::TRUE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_FALSE\0", &boxed::FALSE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_NIL\0", &boxed::NIL_INSTANCE as *const _ as u64);
            jcx.add_symbol(
                b"arret_runtime_alloc_cells\0",
                &compiler_support::alloc_cells as *const _ as u64,
            );

            jcx
        }
    }

    pub fn compile_fun(&mut self, built_funs: &[ops::Fun], fun: &ops::Fun) -> u64 {
        use crate::codegen::fun_gen;

        let tcx = &mut self.tcx;

        self.module_counter += 1;

        let module_counter = self.module_counter;
        let module_name = fun
            .source_name
            .as_ref()
            .map(|source_name| format!("JIT Module #{} for `{}`", module_counter, source_name))
            .unwrap_or_else(|| format!("Anonymous JIT Module #{}", module_counter));

        let module_name_cstring = ffi::CString::new(module_name.as_bytes()).unwrap();
        // Create the module
        let mut mcx = ModCtx::new(tcx, module_name_cstring.as_ref());

        unsafe {
            // TODO: We're regenerating every built fun on each JITed function. This is terrible.
            for (fun_idx, ops_fun) in built_funs.iter().enumerate() {
                let gened_fun = fun_gen::gen_fun(tcx, &mut mcx, ops_fun);
                LLVMSetLinkage(gened_fun.llvm_value, LLVMLinkage::LLVMPrivateLinkage);
                mcx.push_gened_fun(ops::BuiltFunId::new(fun_idx), gened_fun);
            }

            let llvm_function = fun_gen::gen_fun(tcx, &mut mcx, fun).llvm_value;

            // We need to take ownership before we tranfer the module to ORC
            let mut function_name_len: usize = 0;
            let function_name_ptr = LLVMGetValueName2(llvm_function, &mut function_name_len);
            let function_name = ffi::CStr::from_ptr(function_name_ptr).to_owned();

            let module = mcx.into_llvm_module();
            tcx.optimise_module(module);

            let mut orc_module: LLVMOrcModuleHandle = 0;
            if LLVMOrcAddEagerlyCompiledIR(
                self.orc,
                &mut orc_module,
                module,
                Some(orc_sym_resolve),
                self as *mut JITCtx as *mut _,
            ) != LLVMOrcErrorCode::LLVMOrcErrSuccess
            {
                panic!("Unable to add module");
            }

            let mut target_address: LLVMOrcTargetAddress = 0;
            if LLVMOrcGetSymbolAddressIn(
                self.orc,
                &mut target_address,
                orc_module,
                function_name.as_ptr() as *const _,
            ) != LLVMOrcErrorCode::LLVMOrcErrSuccess
            {
                panic!("Unable to get symbol address")
            }

            target_address
        }
    }

    pub fn add_symbol(&mut self, unmangled_name: &[u8], address: u64) {
        unsafe {
            let mut mangled_pointer: *mut libc::c_char = mem::uninitialized();
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
}

impl Drop for JITCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.target_machine);
            LLVMOrcDisposeInstance(self.orc);
        }
    }
}
