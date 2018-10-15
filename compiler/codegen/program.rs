use std::ffi::{CStr, CString};
use std::{env, fs, path, process, ptr};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::context::CodegenCtx;
use crate::codegen::debug_info::DebugInfoBuilder;
use crate::codegen::mod_gen::ModCtx;
use crate::hir::rfi;
use crate::mir;
use crate::mir::ops;
use crate::SourceLoader;

#[derive(Copy, Clone, PartialEq)]
pub enum OutputType {
    LLVMIR,
    Assembly,
    Object,
    Executable,
}

fn task_receiver_llvm_type(cgx: &mut CodegenCtx) -> LLVMTypeRef {
    unsafe {
        let llvm_arg_types = &mut [cgx.task_llvm_ptr_type()];

        LLVMFunctionType(
            LLVMVoidTypeInContext(cgx.llx),
            llvm_arg_types.as_mut_ptr(),
            llvm_arg_types.len() as u32,
            0,
        )
    }
}

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

fn program_to_module(
    source_loader: &mut SourceLoader,
    cgx: &mut CodegenCtx,
    target_machine: LLVMTargetMachineRef,
    program: &mir::BuiltProgram,
) -> LLVMModuleRef {
    use crate::codegen::fun_gen;

    unsafe {
        let module = LLVMModuleCreateWithNameInContext(b"program\0".as_ptr() as *const _, cgx.llx);
        let mut di_builder = DebugInfoBuilder::new(source_loader, program.main.span, module);

        let mut mcx = ModCtx::new(module, target_machine);

        // Build all of our non-main functions
        for (fun_idx, fun) in program.funs.iter().enumerate() {
            let built_fun = fun_gen::gen_fun(cgx, &mut mcx, fun);
            LLVMSetLinkage(built_fun.llvm_value, LLVMLinkage::LLVMPrivateLinkage);
            di_builder.add_function_debug_info(
                fun.span,
                fun.source_name.as_ref(),
                built_fun.llvm_value,
            );

            mcx.push_built_fun(ops::BuiltFunId::new(fun_idx), built_fun);
        }

        // And now the Arret main main
        let arret_main = fun_gen::gen_fun(cgx, &mut mcx, &program.main);
        LLVMSetLinkage(arret_main.llvm_value, LLVMLinkage::LLVMPrivateLinkage);
        di_builder.add_function_debug_info(
            program.main.span,
            program.main.source_name.as_ref(),
            arret_main.llvm_value,
        );

        // Declare our C main
        let builder = LLVMCreateBuilderInContext(cgx.llx);
        let c_main = LLVMAddFunction(
            module,
            b"main\0".as_ptr() as *const _,
            c_main_llvm_type(cgx),
        );
        di_builder.add_function_debug_info(program.main.span, Some(&"main".to_owned()), c_main);

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, c_main, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        if arret_main.takes_task {
            // Declare arret_runtime_launch_task
            let launch_task_llvm_arg_types =
                &mut [LLVMPointerType(task_receiver_llvm_type(cgx), 0)];

            let launch_task_llvm_type = LLVMFunctionType(
                LLVMVoidTypeInContext(cgx.llx),
                launch_task_llvm_arg_types.as_mut_ptr(),
                launch_task_llvm_arg_types.len() as u32,
                0,
            );

            // And launch the task from C main
            let launch_task_llvm_fun = LLVMAddFunction(
                mcx.module,
                "arret_runtime_launch_task\0".as_ptr() as *const _,
                launch_task_llvm_type,
            );

            let launch_task_llvm_args = &mut [arret_main.llvm_value];
            LLVMBuildCall(
                builder,
                launch_task_llvm_fun,
                launch_task_llvm_args.as_mut_ptr(),
                launch_task_llvm_args.len() as u32,
                b"\0".as_ptr() as *const _,
            );
        } else {
            // Call the function directly from the C main without constructing a task
            LLVMBuildCall(
                builder,
                arret_main.llvm_value,
                ptr::null_mut() as *mut _,
                0,
                b"\0".as_ptr() as *const _,
            );
        }

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0));

        LLVMDisposeBuilder(builder);
        di_builder.finalise();

        module
    }
}

/// Generates code for the program with the given output type
///
/// codegen::initialise_llvm() must be called before this.
pub fn gen_program(
    source_loader: &mut SourceLoader,
    rust_libraries: &[rfi::Library],
    program: &mir::BuiltProgram,
    target_triple: Option<&str>,
    output_type: OutputType,
    output_file: &path::Path,
) {
    use crate::codegen::target::create_target_machine;

    let llvm_output_path = if output_type == OutputType::Executable {
        // When outputting an executable this is an intermediate file that we pass to our linker
        output_file.with_extension("o")
    } else {
        // Otherwise this is the final destination
        output_file.to_owned()
    };

    let llvm_output_path_cstring = CString::new(llvm_output_path.to_str().unwrap()).unwrap();

    let mut cgx = CodegenCtx::new();

    let target_machine = create_target_machine(
        target_triple,
        LLVMRelocMode::LLVMRelocDynamicNoPic,
        LLVMCodeModel::LLVMCodeModelDefault,
    );

    let module = program_to_module(source_loader, &mut cgx, target_machine, &program);

    unsafe {
        let mut error: *mut libc::c_char = ptr::null_mut();

        if env::var_os("ARRET_DUMP_LLVM").is_some() {
            LLVMDumpModule(module);
        }

        LLVMVerifyModule(
            module,
            LLVMVerifierFailureAction::LLVMAbortProcessAction,
            &mut error as *mut _,
        );
        LLVMDisposeMessage(error);

        cgx.optimise_module(module);

        let llvm_code_gen_file_type = match output_type {
            OutputType::LLVMIR => {
                if LLVMPrintModuleToFile(
                    module,
                    llvm_output_path_cstring.as_ptr() as *mut _,
                    &mut error as *mut _,
                ) != 0
                {
                    panic!(
                        "LLVMPrintModuleToFile: {}",
                        CStr::from_ptr(error).to_str().unwrap()
                    );
                }
                return;
            }
            OutputType::Assembly => LLVMCodeGenFileType::LLVMAssemblyFile,
            OutputType::Object | OutputType::Executable => LLVMCodeGenFileType::LLVMObjectFile,
        };

        if LLVMTargetMachineEmitToFile(
            target_machine,
            module,
            llvm_output_path_cstring.as_ptr() as *mut _,
            llvm_code_gen_file_type,
            &mut error as *mut _,
        ) != 0
        {
            panic!(
                "LLVMTargetMachineEmitToFile: {}",
                CStr::from_ptr(error).to_str().unwrap()
            );
        }
        LLVMDisposeTargetMachine(target_machine);
    }

    if output_type == OutputType::Executable {
        let target_args = match target_triple {
            Some(triple) => vec!["-target", triple],
            None => vec![],
        };

        let status = process::Command::new("cc")
            .arg(llvm_output_path.clone())
            .args(target_args)
            .arg("-o")
            .arg(output_file)
            .args(rust_libraries.iter().map(|l| l.target_path()))
            .arg("-pthread")
            .arg("-ldl")
            .status()
            .unwrap();

        let _ = fs::remove_file(llvm_output_path);

        if !status.success() {
            panic!("Error invoking linker");
        }
    }
}
