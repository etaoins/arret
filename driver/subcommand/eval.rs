use compiler::error::Error;
use compiler::reporting::report_to_stderr;
use compiler::CompileCtx;

fn try_eval_input_file(ccx: &CompileCtx, input_file: &compiler::SourceFile) -> Result<(), Error> {
    let hir = compiler::lower_program(ccx, input_file)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    ehx.eval_main_fun(hir.main_var_id)?;
    Ok(())
}

pub fn eval_input_file(ccx: &CompileCtx, input_file: &compiler::SourceFile) -> bool {
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
