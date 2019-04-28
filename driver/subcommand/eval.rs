use arret_compiler::error::Error;
use arret_compiler::reporting::report_to_stderr;
use arret_compiler::CompileCtx;

fn try_eval_input_file(
    ccx: &CompileCtx,
    input_file: &arret_compiler::SourceFile,
) -> Result<(), Error> {
    let hir = arret_compiler::lower_program(ccx, input_file)?;
    let inferred_defs = arret_compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = arret_compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    ehx.eval_main_fun(hir.main_var_id)?;
    Ok(())
}

pub fn eval_input_file(ccx: &CompileCtx, input_file: &arret_compiler::SourceFile) -> bool {
    let result = try_eval_input_file(ccx, input_file);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(ccx.source_loader(), &*err);
        }
        false
    } else {
        true
    }
}
