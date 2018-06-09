#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate ansi_term;
extern crate syntax;

#[macro_use]
mod id_type;

mod ctx;
mod hir;
pub mod reporting;
mod ty;
mod typeck;

pub use ctx::CompileContext;
pub use hir::lowering::lower_program;
pub use typeck::infer::infer_program;
