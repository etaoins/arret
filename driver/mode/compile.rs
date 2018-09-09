use std::path;

use compiler::error::Error;

use crate::DriverConfig;

use compiler::reporting::report_to_stderr;

fn try_compile_input_file(
    cfg: &DriverConfig,
    source_loader: &mut compiler::SourceLoader,
    input_path: &path::Path,
) -> Result<(), Error> {
    let source_file_id = source_loader
        .load_path(input_path)
        .expect("Unable to read input file");

    let hir = compiler::lower_program(&cfg.package_paths, source_loader, source_file_id)?;
    let inferred_defs = compiler::infer_program(&hir.pvars, &hir.tvars, hir.module_defs)?;

    let mut pcx = compiler::PartialEvalCtx::new();

    for inferred_def in inferred_defs {
        println!("{:?}", inferred_def);
        pcx.consume_def(&hir.tvars, inferred_def)?;
    }

    Ok(())
}

pub fn compile_input_file(cfg: &DriverConfig, input_path: &path::Path) -> bool {
    let mut source_loader = compiler::SourceLoader::new();
    let result = try_compile_input_file(cfg, &mut source_loader, input_path);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(&source_loader, &*err);
        }
        false
    } else {
        true
    }
}
