use codespan_reporting::Diagnostic;

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx};

fn try_eval_input_file(
    ccx: &CompileCtx,
    input_file: &arret_compiler::SourceFile,
) -> Result<(), Vec<Diagnostic>> {
    let arret_compiler::InferredProgram {
        defs, main_var_id, ..
    } = arret_compiler::program_to_inferred_hir(ccx, input_file)?;

    let mut ehx = arret_compiler::EvalHirCtx::new(true);
    for def in defs {
        ehx.consume_def(def)?;
    }

    ehx.eval_main_fun(main_var_id)?;

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
