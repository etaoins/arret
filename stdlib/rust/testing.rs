use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;

use rfi_derive;

#[rfi_derive::rust_fun("(All #{T} T -> T)")]
pub fn stdlib_black_box(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}

#[rfi_derive::rust_fun("(All #{T} T ->! T)")]
pub fn stdlib_black_box_impure(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}