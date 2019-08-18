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

pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, Options as GenProgramOptions, OutputType};
pub use crate::context::CompileCtx;
pub use crate::hir::lowering::lower_program;
pub use crate::hir::PackagePaths;
pub use crate::id_type::ArcId;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::mir::print_program as print_program_mir;
pub use crate::reporting::{emit_diagnostics_to_stderr, errors_to_diagnostics};
pub use crate::source::{SourceFile, SourceLoader};
pub use crate::typeck::infer::infer_program;
