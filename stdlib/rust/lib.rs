#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
extern crate runtime;

mod pretty_print;
use crate::pretty_print::pretty_print;

pub mod list;
use crate::list::*;
pub mod testing;
use crate::testing::*;
pub mod write;
use crate::write::*;

use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

define_rust_fn! {
    #[arret_type="(Any ... -> (U))"]
    PANIC = fn arret_stdlib_panic(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> Never {
        use std::str;

        let mut output = Vec::<u8>::new();
        for value in values.iter() {
            pretty_print(&mut output, task, value)
        }

        let message = str::from_utf8(output.as_slice()).unwrap().to_owned();
        task.panic(message)
    }
}

define_rust_fn! {
    #[arret_type="(Int ->! (U))"]
    EXIT = fn arret_stdlib_exit(exit_code: i64) -> () {
        use std::process::exit;
        exit(exit_code as i32);
    }
}

define_rust_module!(ARRET_STDLIB_RUST_EXPORTS, {
    "length" => LENGTH,
    "panic" => PANIC,
    "print!" => PRINT,
    "println!" => PRINTLN,
    "exit!" => EXIT,
    "cons" => CONS,
    "black-box" => BLACK_BOX,
    "black-box!" => BLACK_BOX_IMPURE
});
