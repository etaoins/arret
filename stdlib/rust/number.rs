use runtime::binding::*;

use rfi_derive;

#[rfi_derive::rust_fun("(Float -> Bool)")]
pub fn stdlib_nan_p(float: f64) -> bool {
    float.is_nan()
}
