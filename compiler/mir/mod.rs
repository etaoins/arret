mod app_purity;
mod arg_list;
mod builder;
mod closure;
mod costing;
mod equality;
pub mod error;
pub mod eval_hir;
mod inliner;
mod intrinsic;
pub mod ops;
mod optimise;
mod polymorph;
mod record_field;
mod ret_value;
mod rust_fun;
mod specific_abi_type;
mod tagset;
mod typred;
mod value;

pub use crate::mir::eval_hir::BuiltProgram;
pub use crate::mir::value::Value;

use crate::hir;
type Expr = hir::Expr<hir::Inferred>;
