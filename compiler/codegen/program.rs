use std::ffi::{CStr, CString};
use std::{fs, path, process, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;

use crate::codegen::fun_gen::{gen_op, FunCtx};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::hir::rfi;
use crate::mir::ops::Op;

fn c_main_llvm_type(cgx: &mut CodegenCtx) -> LLVMTypeRef {
    unsafe {
        let llvm_argc_type = LLVMInt32TypeInContext(cgx.llx);
        let llvm_argv_type = LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(cgx.llx), 0), 0);
        let llvm_ret_type = LLVMInt32TypeInContext(cgx.llx);

        let llvm_arg_types = &mut [llvm_argc_type, llvm_argv_type];

        LLVMFunctionType(
            llvm_ret_type,
            llvm_arg_types.as_mut_ptr(),
            llvm_arg_types.len() as u32,
            0,
        )
    }
}

fn program_to_module(cgx: &mut CodegenCtx, program: Vec<Op>) -> LLVMModuleRef {
    unsafe {
        let module = LLVMModuleCreateWithNameInContext(b"program\0".as_ptr() as *const _, cgx.llx);
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let function = LLVMAddFunction(
            module,
            b"main\0".as_ptr() as *const _,
            c_main_llvm_type(cgx),
        );

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut mcx = ModCtx::new(module);
        let mut fcx = FunCtx::new(function, builder);

        for op in program {
            gen_op(cgx, &mut mcx, &mut fcx, &op);
        }

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0));

        module
    }
}

pub fn gen_program(rust_libraries: &[rfi::Library], program: Vec<Op>, output_file: &path::Path) {
    use crate::codegen::target::default_target_machine;
    let object_path = output_file.with_extension("o");

    let mut cgx = CodegenCtx::new();

    let target_machine = default_target_machine(
        LLVMRelocMode::LLVMRelocDynamicNoPic,
        LLVMCodeModel::LLVMCodeModelDefault,
    );
    let module = program_to_module(&mut cgx, program);

    unsafe {
        let error: *mut *mut libc::c_char = ptr::null_mut();
        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            error,
        );

        //LLVMDumpModule(module);

        let object_path_cstring = CString::new(object_path.to_str().unwrap()).unwrap();
        if LLVMTargetMachineEmitToFile(
            target_machine,
            module,
            object_path_cstring.as_ptr() as *mut _,
            LLVMCodeGenFileType::LLVMObjectFile,
            error,
        ) != 0
        {
            panic!(CStr::from_ptr(*error));
        }
    }

    let status = process::Command::new("cc")
        .arg(object_path.clone())
        .arg("-o")
        .arg(output_file)
        .args(rust_libraries.iter().map(|l| l.path()))
        .status()
        .unwrap();

    let _ = fs::remove_file(object_path);

    if !status.success() {
        panic!("Error invoking linker");
    }
}
