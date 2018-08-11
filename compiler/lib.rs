#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]

#[macro_use]
mod id_type;

mod hir;
pub mod repl;
pub mod reporting;
mod source;
mod ty;
mod typeck;

pub use crate::hir::lowering::lower_program;
pub use crate::hir::PackagePaths;
pub use crate::source::{SourceFile, SourceFileId, SourceKind, SourceLoader};
pub use crate::typeck::infer::infer_program;
