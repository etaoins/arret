use std::ffi::{CStr, CString};
use std::{fs, path, process, ptr};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMLinkage;

use crate::codegen::analysis::AnalysedMod;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::hir::rfi;
use crate::mir;
use crate::SourceLoader;

#[derive(Copy, Clone, PartialEq)]
pub enum OutputType {
    LLVMIR,
    Assembly,
    Object,
    Executable,
}

#[derive(Copy, Clone, PartialEq)]
pub struct Options<'target> {
    target_triple: Option<&'target str>,
    output_type: OutputType,
    llvm_opt: bool,
}

impl<'target> Options<'target> {
    pub fn new() -> Options<'static> {
        Options {
            target_triple: None,
            output_type: OutputType::Executable,
            llvm_opt: true,
        }
    }

    pub fn with_target_triple(self, target_triple: Option<&'target str>) -> Options<'target> {
        Options {
            target_triple,
            ..self
        }
    }

    pub fn with_llvm_opt(self, llvm_opt: bool) -> Options<'target> {
        Options { llvm_opt, ..self }
    }

    pub fn with_output_type(self, output_type: OutputType) -> Options<'target> {
        Options {
            output_type,
            ..self
        }
    }
}

impl Default for Options<'static> {
    fn default() -> Options<'static> {
        Options::new()
    }
}

fn task_receiver_llvm_type(tcx: &mut TargetCtx) -> LLVMTypeRef {
    unsafe {
        let llvm_arg_types = &mut [tcx.task_llvm_ptr_type()];

        LLVMFunctionType(
            LLVMVoidTypeInContext(tcx.llx),
            llvm_arg_types.as_mut_ptr(),
            llvm_arg_types.len() as u32,
            0,
        )
    }
}

fn c_main_llvm_type(tcx: &mut TargetCtx) -> LLVMTypeRef {
    unsafe {
        let llvm_argc_type = LLVMInt32TypeInContext(tcx.llx);
        let llvm_argv_type = LLVMPointerType(LLVMPointerType(LLVMInt8TypeInContext(tcx.llx), 0), 0);
        let llvm_ret_type = LLVMInt32TypeInContext(tcx.llx);

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
    tcx: &mut TargetCtx,
    program: &mir::BuiltProgram,
    debug_source_loader: Option<&SourceLoader>,
) -> LLVMModuleRef {
    unsafe {
        let analysed_mod = AnalysedMod::new(&program.private_funs, &program.main);

        let mut mcx = ModCtx::new(
            tcx,
            CString::new("program").unwrap().as_ref(),
            &analysed_mod,
            debug_source_loader,
        );

        // And now the Arret main main
        let llvm_arret_main = mcx.llvm_entry_fun(tcx);
        LLVMSetLinkage(llvm_arret_main, LLVMLinkage::LLVMPrivateLinkage);

        // Declare our C main
        let builder = LLVMCreateBuilderInContext(tcx.llx);
        let c_main = LLVMAddFunction(
            mcx.module,
            b"main\0".as_ptr() as *const _,
            c_main_llvm_type(tcx),
        );

        if let Some(di_builder) = mcx.di_builder() {
            di_builder.add_function_debug_info(program.main.span, Some(&"main".to_owned()), c_main);
        }

        let bb = LLVMAppendBasicBlockInContext(tcx.llx, c_main, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        // Declare arret_runtime_launch_task
        let launch_task_llvm_arg_types = &mut [LLVMPointerType(task_receiver_llvm_type(tcx), 0)];

        let launch_task_llvm_type = LLVMFunctionType(
            LLVMVoidTypeInContext(tcx.llx),
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

        let launch_task_llvm_args = &mut [llvm_arret_main];
        LLVMBuildCall(
            builder,
            launch_task_llvm_fun,
            launch_task_llvm_args.as_mut_ptr(),
            launch_task_llvm_args.len() as u32,
            b"\0".as_ptr() as *const _,
        );

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), 0, 0));

        LLVMDisposeBuilder(builder);
        mcx.into_llvm_module(tcx)
    }
}

/// Generates code for the program with the given output type
///
/// codegen::initialise_llvm() must be called before this.
pub fn gen_program(
    options: Options<'_>,
    rust_libraries: &[rfi::Library],
    program: &mir::BuiltProgram,
    output_file: &path::Path,
    debug_source_loader: Option<&SourceLoader>,
) {
    use crate::codegen::target_machine::create_target_machine;

    let Options {
        target_triple,
        output_type,
        llvm_opt,
    } = options;

    let llvm_output_path = if output_type == OutputType::Executable {
        // When outputting an executable this is an intermediate file that we pass to our linker
        output_file.with_extension("o")
    } else {
        // Otherwise this is the final destination
        output_file.to_owned()
    };

    let llvm_output_path_cstring = CString::new(llvm_output_path.to_str().unwrap()).unwrap();

    let target_machine = create_target_machine(
        target_triple,
        LLVMRelocMode::LLVMRelocDynamicNoPic,
        LLVMCodeModel::LLVMCodeModelDefault,
    );

    let mut tcx = TargetCtx::new(target_machine, llvm_opt);
    let module = program_to_module(&mut tcx, &program, debug_source_loader);
    tcx.optimise_module(module);

    unsafe {
        let mut error: *mut libc::c_char = ptr::null_mut();

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
