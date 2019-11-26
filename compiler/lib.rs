#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

mod codegen;
mod context;
mod hir;
mod mir;
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

pub struct InferredProgram {
    pub defs: Vec<hir::Def<hir::Inferred>>,
    pub main_var_id: hir::VarId,
    pub rust_libraries: Vec<Arc<rfi::Library>>,
}

pub fn program_to_inferred_hir(
    ccx: &CompileCtx,
    source_file: &SourceFile,
) -> Result<InferredProgram, Vec<Diagnostic>> {
    use crate::reporting::errors_to_diagnostics;

    let hir = hir::lowering::lower_program(ccx, source_file).map_err(errors_to_diagnostics)?;

    let inferred_defs = typeck::infer::infer_program_defs(hir.defs, hir.main_var_id)
        .map_err(errors_to_diagnostics)?;

    Ok(InferredProgram {
        defs: inferred_defs,
        main_var_id: hir.main_var_id,
        rust_libraries: hir.rust_libraries,
    })
}
