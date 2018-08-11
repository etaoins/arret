#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]

#[macro_use]
extern crate runtime;

use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;

define_rust_fn! {
    #[arret-type="((Listof Any) -> Int)"]
    LENGTH = fn length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
        input.len() as i64
    }
}

define_rust_module! {
    "length" => LENGTH
}
