mod builder;
mod compact_abi_type;
pub mod error;
pub mod eval_hir;
mod intrinsic;
pub(crate) mod ops;
mod optimise;
mod rust_fun;
mod value;

pub use crate::mir::eval_hir::BuiltProgram;
pub use crate::mir::value::Value;

use crate::hir;
type Expr = hir::Expr<hir::Inferred>;
