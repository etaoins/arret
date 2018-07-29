#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate ansi_term;
extern crate libloading;
extern crate runtime;
extern crate syntax;

#[macro_use]
mod id_type;

mod hir;
pub mod reporting;
mod source;
mod ty;
mod typeck;

pub use hir::lowering::lower_program;
pub use hir::PackagePaths;
pub use source::{SourceFile, SourceFileId, SourceLoader};
pub use typeck::infer::infer_program;
