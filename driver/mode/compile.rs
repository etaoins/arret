use std::path;

use compiler::error::Error;

use crate::DriverConfig;

use compiler::reporting::report_to_stderr;

fn try_compile_input_file(
    cfg: &DriverConfig,
    source_loader: &mut compiler::SourceLoader,
    options: compiler::GenProgramOptions<'_>,
    input_path: &path::Path,
    output_path: &path::Path,
) -> Result<(), Error> {
    let source_file_id = source_loader
        .load_path(input_path)
        .expect("Unable to read input file");

    let hir = compiler::lower_program(&cfg.package_paths, source_loader, source_file_id)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    let mir_program = ehx.into_built_program(hir.main_var_id)?;
    compiler::gen_program(
        source_loader,
        options,
        &hir.rust_libraries,
        &mir_program,
        output_path,
    );

    Ok(())
}

pub fn compile_input_file(
    cfg: &DriverConfig,
    input_path: &path::Path,
    target_triple: Option<&str>,
    output_path: &path::Path,
) -> bool {
    let mut source_loader = compiler::SourceLoader::new();

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

    let result = try_compile_input_file(cfg, &mut source_loader, options, input_path, output_path);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(&source_loader, &*err);
        }
        false
    } else {
        true
    }
}
