#![feature(tool_lints)]
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

pub use crate::hir::lowering::lower_program;
pub use crate::hir::PackagePaths;
pub use crate::mir::eval_hir::{EvalHirCtx, EvalMode as EvalHirMode};
pub use crate::source::{SourceFile, SourceFileId, SourceKind, SourceLoader};
pub use crate::typeck::infer::infer_program;
