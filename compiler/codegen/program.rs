use std::ffi::{CStr, CString};
use std::{fs, path, process, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::hir::rfi;
use crate::mir;

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

fn program_to_module(cgx: &mut CodegenCtx, program: mir::BuiltProgram) -> LLVMModuleRef {
    use crate::codegen::fun_gen;

    unsafe {
        let module = LLVMModuleCreateWithNameInContext(b"program\0".as_ptr() as *const _, cgx.llx);
        let mut mcx = ModCtx::new(module);

        // Build the Arret main
        let arret_main_symbol = CString::new("arret_main").unwrap();
        let arret_main_llvm_value =
            fun_gen::gen_fun(cgx, &mut mcx, &arret_main_symbol, &program.main);
        LLVMSetLinkage(arret_main_llvm_value, LLVMLinkage::LLVMPrivateLinkage);

        // And all of the other functions
        for (fun_symbol, fun) in program.funs {
            let fun_llvm_value =
                fun_gen::gen_fun(cgx, &mut mcx, &CString::new(fun_symbol).unwrap(), &fun);
            LLVMSetLinkage(fun_llvm_value, LLVMLinkage::LLVMPrivateLinkage)
        }

        // Declare arret_launch_task
        let launch_task_llvm_arg_types = &mut [LLVMTypeOf(arret_main_llvm_value)];
        let launch_task_llvm_type = LLVMFunctionType(
            LLVMVoidTypeInContext(cgx.llx),
            launch_task_llvm_arg_types.as_mut_ptr(),
            launch_task_llvm_arg_types.len() as u32,
            0,
        );

        let launch_task_llvm_fun = LLVMAddGlobal(
            mcx.module,
            launch_task_llvm_type,
            "arret_runtime_launch_task\0".as_ptr() as *const _,
        );

        // Now build our C main
        let builder = LLVMCreateBuilderInContext(cgx.llx);
        let function = LLVMAddFunction(
            module,
            b"main\0".as_ptr() as *const _,
            c_main_llvm_type(cgx),
        );

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let launch_task_llvm_args = &mut [arret_main_llvm_value];
        LLVMBuildCall(
            builder,
            launch_task_llvm_fun,
            launch_task_llvm_args.as_mut_ptr(),
            launch_task_llvm_args.len() as u32,
            b"\0".as_ptr() as *const _,
        );

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0));

        module
    }
}

pub fn gen_program(
    rust_libraries: &[rfi::Library],
    program: mir::BuiltProgram,
    output_file: &path::Path,
) {
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
