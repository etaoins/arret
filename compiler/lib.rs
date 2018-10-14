#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

mod codegen;
pub mod error;
mod hir;
mod mir;
pub mod repl;
pub mod reporting;
mod source;
mod ty;
mod typeck;

pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, OutputType};
pub use crate::hir::lowering::lower_program;
pub use crate::hir::PackagePaths;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::source::{SourceFile, SourceFileId, SourceKind, SourceLoader};
pub use crate::typeck::infer::infer_program;
