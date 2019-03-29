use std::path;

use compiler::error::Error;
use compiler::reporting::report_to_stderr;

use crate::DriverConfig;

fn try_compile_input_file(
    cfg: &mut DriverConfig,
    options: compiler::GenProgramOptions<'_>,
    input_file: &compiler::SourceFile,
    output_path: &path::Path,
    debug_info: bool,
) -> Result<(), Error> {
    let hir = compiler::lower_program(&cfg.package_paths, &mut cfg.source_loader, input_file)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    if ehx.should_collect() {
        ehx.collect_garbage();
    }

    let mir_program = ehx.into_built_program(hir.main_var_id)?;

    let debug_source_loader = if debug_info {
        Some(&cfg.source_loader)
    } else {
        None
    };

    compiler::gen_program(
        options,
        &hir.rust_libraries,
        &mir_program,
        output_path,
        debug_source_loader,
    );

    Ok(())
}

pub fn compile_input_file(
    cfg: &mut DriverConfig,
    input_file: &compiler::SourceFile,
    target_triple: Option<&str>,
    output_path: &path::Path,
    debug_info: bool,
) -> bool {
    let output_type = match output_path.extension().and_then(|e| e.to_str()) {
        Some("ll") => compiler::OutputType::LLVMIR,
        Some("s") => compiler::OutputType::Assembly,
        Some("o") => compiler::OutputType::Object,
        _ => compiler::OutputType::Executable,
    };

    let options = compiler::GenProgramOptions::new()
        .with_target_triple(target_triple)
        .with_output_type(output_type)
        .with_llvm_opt(cfg.llvm_opt);

    let result = try_compile_input_file(cfg, options, input_file, output_path, debug_info);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(&cfg.source_loader, &*err);
        }
        false
    } else {
        true
    }
}
