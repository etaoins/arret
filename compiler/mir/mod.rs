pub mod error;
mod intrinsic;
pub mod partial_eval;
mod value;

use crate::hir;
pub use crate::mir::value::Value;
use crate::ty;

type Expr = hir::Expr<ty::Poly>;
