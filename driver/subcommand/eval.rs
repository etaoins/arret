use compiler::error::Error;
use compiler::reporting::report_to_stderr;

use crate::DriverConfig;

fn try_eval_input_file(
    cfg: &mut DriverConfig,
    input_file_id: compiler::SourceFileId,
) -> Result<(), Error> {
    let hir = compiler::lower_program(&cfg.package_paths, &mut cfg.source_loader, input_file_id)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    ehx.eval_main_fun(hir.main_var_id)?;
    Ok(())
}

pub fn eval_input_file(cfg: &mut DriverConfig, input_file_id: compiler::SourceFileId) -> bool {
    let result = try_eval_input_file(cfg, input_file_id);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(&cfg.source_loader, &*err);
        }
        false
    } else {
        true
    }
}
