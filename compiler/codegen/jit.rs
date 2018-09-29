use std::collections::HashMap;
use std::{env, ffi, mem};

use runtime::boxed;

use libc;

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::orc::*;
use llvm_sys::target_machine::*;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops;

extern "C" fn orc_sym_resolve(name_ptr: *const libc::c_char, jcx_void: *mut libc::c_void) -> u64 {
    unsafe {
        let jcx: &JITCtx = &*(jcx_void as *mut _);
        let name = ffi::CStr::from_ptr(name_ptr);
        jcx.symbols.get(name).cloned().unwrap_or(0)
    }
}

pub struct JITCtx {
    orc: LLVMOrcJITStackRef,
    symbols: HashMap<ffi::CString, u64>,
}

impl JITCtx {
    pub fn new() -> JITCtx {
        unsafe {
            use crate::codegen::target::create_target_machine;

            LLVMLinkInMCJIT();

            let target_machine = create_target_machine(
                None, // Can't cross compile in the JIT
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelJITDefault,
            );
            let orc = LLVMOrcCreateInstance(target_machine);

            let mut jcx = JITCtx {
                orc,
                symbols: HashMap::new(),
            };

            jcx.add_symbol(b"ARRET_TRUE\0", &boxed::TRUE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_FALSE\0", &boxed::FALSE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_NIL\0", &boxed::NIL_INSTANCE as *const _ as u64);

            jcx
        }
    }

    pub fn compile_fun(&mut self, cgx: &mut CodegenCtx, fun: &ops::Fun) -> u64 {
        use crate::codegen::fun_gen;
        use std::ptr;

        let module_name = fun
            .source_name
            .as_ref()
            .map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap())
            .unwrap_or_else(|| ffi::CString::new("anon_mod").unwrap());

        unsafe {
            // Create the module
            let module = LLVMModuleCreateWithNameInContext(
                module_name.as_bytes_with_nul().as_ptr() as *const _,
                cgx.llx,
            );

            let mut mcx = ModCtx::new(module);
            let llvm_function = fun_gen::gen_fun(cgx, &mut mcx, fun);

            // We need to take ownership before we tranfer the module to ORC
            let mut function_name_len: usize = 0;
            let function_name_ptr = LLVMGetValueName2(llvm_function, &mut function_name_len);
            let function_name = ffi::CStr::from_ptr(function_name_ptr).to_owned();

            if env::var_os("ARRET_DUMP_LLVM").is_some() {
                LLVMDumpModule(module);
            }

            let error: *mut *mut libc::c_char = ptr::null_mut();
            LLVMVerifyModule(
                module,
                LLVMVerifierFailureAction::LLVMAbortProcessAction,
                error,
            );

            let mut orc_module: LLVMOrcModuleHandle = 0;
            if LLVMOrcAddLazilyCompiledIR(
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
            LLVMOrcDisposeInstance(self.orc);
        }
    }
}
