use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::span::FileId;

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx};

fn try_eval_input_file(
    ccx: &CompileCtx,
    input_file: &arret_compiler::SourceFile,
) -> Result<(), Vec<Diagnostic<FileId>>> {
    let arret_compiler::EvaluableProgram {
        mut ehx,
        main_export_id,
        ..
    } = arret_compiler::program_to_evaluable(ccx, input_file)?;

    ehx.eval_main_fun(main_export_id)?;

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
