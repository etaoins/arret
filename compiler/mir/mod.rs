mod arg_list;
mod builder;
mod closure;
mod compact_abi_type;
mod equality;
pub mod error;
pub mod eval_hir;
mod intrinsic;
pub(crate) mod ops;
mod optimise;
mod ret_value;
mod rust_fun;
mod tagset;
mod typred;
mod value;

pub use crate::mir::eval_hir::BuiltProgram;
pub use crate::mir::value::Value;

use crate::hir;
type Expr = hir::Expr<hir::Inferred>;
