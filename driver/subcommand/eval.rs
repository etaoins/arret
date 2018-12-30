use std::path;

use compiler::error::Error;
use compiler::reporting::report_to_stderr;

use crate::DriverConfig;

fn try_eval_input_file(
    cfg: &DriverConfig,
    source_loader: &mut compiler::SourceLoader,
    input_path: &path::Path,
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

    ehx.eval_main_fun(hir.main_var_id)?;
    Ok(())
}

pub fn eval_input_file(cfg: &DriverConfig, input_path: &path::Path) -> bool {
    let mut source_loader = compiler::SourceLoader::new();

    let result = try_eval_input_file(cfg, &mut source_loader, input_path);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(&source_loader, &*err);
        }
        false
    } else {
        true
    }
}
