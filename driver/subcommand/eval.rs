use codespan_reporting::Diagnostic;

use arret_compiler::{emit_diagnostics_to_stderr, errors_to_diagnostics, CompileCtx};

fn try_eval_input_file(
    ccx: &CompileCtx,
    input_file: &arret_compiler::SourceFile,
) -> Result<(), Vec<Diagnostic>> {
    let hir = arret_compiler::lower_program(ccx, input_file).map_err(errors_to_diagnostics)?;

    let inferred_defs =
        arret_compiler::infer_program(hir.defs, hir.main_var_id).map_err(errors_to_diagnostics)?;

    let mut ehx = arret_compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    ehx.eval_main_fun(hir.main_var_id)?;

    Ok(())
}

pub fn eval_input_file(ccx: &CompileCtx, input_file: &arret_compiler::SourceFile) -> bool {
    let result = try_eval_input_file(ccx, input_file);

    if let Err(diagnostics) = result {
        emit_diagnostics_to_stderr(ccx.source_loader(), diagnostics);
        false
    } else {
        true
    }
}
