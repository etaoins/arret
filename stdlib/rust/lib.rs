#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]
#![warn(rust_2018_idioms)]

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

define_rust_fn! {
    #[arret-type="(Any -> (U))"]
    PANIC = fn panic(input: Gc<boxed::Any>) -> Never {
        match input.as_subtype() {
            boxed::AnySubtype::Str(s) => {
                panic!("Arret panic: {}", s.as_str());
            }
            _ => {
                panic!("Arret non-string panic")
            }

        }
    }
}

define_rust_module! {
    "length" => LENGTH,
    "panic" => PANIC
}
