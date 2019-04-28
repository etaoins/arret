use std::path;

use arret_compiler::error::Error;
use arret_compiler::reporting::report_to_stderr;
use arret_compiler::CompileCtx;

fn try_compile_input_file(
    ccx: &CompileCtx,
    options: arret_compiler::GenProgramOptions<'_>,
    input_file: &arret_compiler::SourceFile,
    output_path: &path::Path,
    debug_info: bool,
) -> Result<(), Error> {
    let hir = arret_compiler::lower_program(ccx, input_file)?;
    let inferred_defs = arret_compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = arret_compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    if ehx.should_collect() {
        ehx.collect_garbage();
    }

    let mir_program = ehx.into_built_program(hir.main_var_id)?;

    let debug_source_loader = if debug_info {
        Some(ccx.source_loader())
    } else {
        None
    };

    arret_compiler::gen_program(
        options,
        &hir.rust_libraries,
        &mir_program,
        output_path,
        debug_source_loader,
    );

    Ok(())
}

pub fn compile_input_file(
    ccx: &CompileCtx,
    input_file: &arret_compiler::SourceFile,
    target_triple: Option<&str>,
    output_path: &path::Path,
    debug_info: bool,
) -> bool {
    use std::ffi;

    let output_type = match output_path.extension().and_then(ffi::OsStr::to_str) {
        Some("ll") => arret_compiler::OutputType::LLVMIR,
        Some("s") => arret_compiler::OutputType::Assembly,
        Some("o") => arret_compiler::OutputType::Object,
        _ => arret_compiler::OutputType::Executable,
    };

    let options = arret_compiler::GenProgramOptions::new()
        .with_target_triple(target_triple)
        .with_output_type(output_type)
        .with_llvm_opt(ccx.enable_optimisations());

    let result = try_compile_input_file(ccx, options, input_file, output_path, debug_info);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(ccx.source_loader(), &*err);
        }
        false
    } else {
        true
    }
}
