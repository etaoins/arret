use std::collections::HashMap;
use std::{ffi, mem, ptr};

use runtime::boxed;

use libc;

use llvm_sys::execution_engine::*;
use llvm_sys::orc::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

extern "C" fn orc_sym_resolve(name_ptr: *const libc::c_char, jcx_void: *mut libc::c_void) -> u64 {
    unsafe {
        let jcx: &JITCtx = &*(jcx_void as *mut _);
        let name = ffi::CStr::from_ptr(name_ptr);
        jcx.symbols.get(name).cloned().unwrap_or(0)
    }
}

pub struct JITCtx {
    target_machine: LLVMTargetMachineRef,
    orc: LLVMOrcJITStackRef,

    symbols: HashMap<ffi::CString, u64>,
}

impl JITCtx {
    pub fn new() -> JITCtx {
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
            LLVMLinkInMCJIT();

            let default_triple = LLVMGetDefaultTargetTriple();

            let mut target: LLVMTargetRef = mem::uninitialized();
            if LLVMGetTargetFromTriple(default_triple, &mut target, ptr::null_mut()) != 0 {
                panic!("Can't get target triple");
            }

            let target_machine = LLVMCreateTargetMachine(
                target,
                default_triple,
                ptr::null(),
                ptr::null(),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelJITDefault,
            );

            let orc = LLVMOrcCreateInstance(target_machine);

            let mut jcx = JITCtx {
                target_machine,
                orc,

                symbols: HashMap::new(),
            };

            jcx.add_symbol(b"ARRET_TRUE\0", &boxed::TRUE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_FALSE\0", &boxed::FALSE_INSTANCE as *const _ as u64);
            jcx.add_symbol(b"ARRET_NIL\0", &boxed::NIL_INSTANCE as *const _ as u64);

            jcx
        }
    }

    pub fn compile_fun(&mut self, module: LLVMModuleRef, fun_name: &[u8]) -> u64 {
        unsafe {
            let shared_module = LLVMOrcMakeSharedModule(module);

            let mut orc_module: LLVMOrcModuleHandle = mem::uninitialized();
            if LLVMOrcAddLazilyCompiledIR(
                self.orc,
                &mut orc_module,
                shared_module,
                Some(orc_sym_resolve),
                self as *mut JITCtx as *mut _,
            ) != LLVMOrcErrorCode::LLVMOrcErrSuccess
            {
                panic!("Unable to add module");
            }

            let mut target_address: LLVMOrcTargetAddress = 0;
            if LLVMOrcGetSymbolAddress(self.orc, &mut target_address, fun_name.as_ptr() as *const _)
                != LLVMOrcErrorCode::LLVMOrcErrSuccess
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
