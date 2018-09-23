mod builder;
pub mod error;
pub mod eval_hir;
mod intrinsic;
pub(crate) mod ops;
mod rust_fun;
mod value;

use crate::hir;
pub use crate::mir::value::Value;
use crate::ty;

type Expr = hir::Expr<ty::Poly>;
