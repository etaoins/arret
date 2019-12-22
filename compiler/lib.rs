#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

mod codegen;
mod context;
mod hir;
mod mir;
mod promise;
pub mod repl;
mod reporting;
mod rfi;
mod source;
mod ty;
mod typeck;

use codespan_reporting::Diagnostic;
use std::sync::Arc;

pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, Options as GenProgramOptions, OutputType};
pub use crate::context::CompileCtx;
pub use crate::hir::PackagePaths;
pub use crate::id_type::ArcId;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::mir::print_program as print_program_mir;
pub use crate::reporting::emit_diagnostics_to_stderr;
pub use crate::source::{SourceFile, SourceLoader};

pub struct EvaluableProgram {
    pub ehx: EvalHirCtx,
    pub main_var_id: hir::VarId,
    pub rust_libraries: Vec<Arc<rfi::Library>>,
}

pub fn program_to_evaluable(
    ccx: &CompileCtx,
    source_file: &SourceFile,
) -> Result<EvaluableProgram, Vec<Diagnostic>> {
    use crate::reporting::errors_to_diagnostics;

    let hir = hir::lowering::lower_program(ccx, source_file).map_err(errors_to_diagnostics)?;

    let inferred_defs = typeck::infer::infer_program_defs(hir.module_defs, hir.main_var_id)
        .map_err(errors_to_diagnostics)?;

    let mut ehx = EvalHirCtx::new(ccx.enable_optimisations());

    for def in inferred_defs {
        ehx.consume_def(def)?;
    }

    if ehx.should_collect() {
        ehx.collect_garbage();
    }

    Ok(EvaluableProgram {
        ehx,
        main_var_id: hir.main_var_id,
        rust_libraries: hir.rust_libraries,
    })
}
