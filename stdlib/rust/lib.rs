#![feature(tool_lints)]
#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
extern crate runtime;

mod pprint;

use std::io;
use std::io::prelude::*;

use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

define_rust_fn! {
    #[arret_type="((Listof Any) -> Int)"]
    LENGTH = fn arret_stdlib_length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
        input.len() as i64
    }
}

define_rust_fn! {
    #[arret_type="(Any ... -> (U))"]
    PANIC = fn arret_stdlib_panic(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> Never {
        use std::str;

        let mut output = Vec::<u8>::new();
        for value in values.iter() {
            pprint::pretty_print(&mut output, task, value)
        }

        let message = str::from_utf8(output.as_slice()).unwrap().to_owned();
        task.panic(message)
    }
}

define_rust_fn! {
    #[arret_type="(Any Any -> Bool)"]
    EQUALS = fn arret_stdlib_equals(lhs: Gc<boxed::Any>, rhs: Gc<boxed::Any>) -> bool {
        lhs == rhs
    }
}

define_rust_fn! {
    #[arret_type="(Any ... ->! ())"]
    PRINT = fn arret_stdlib_print(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> () {
        let mut output = io::stdout();

        for value in values.iter() {
            pprint::pretty_print(&mut output, task, value);
        }
    }
}

define_rust_fn! {
    #[arret_type="(Any ... ->! ())"]
    PRINTLN = fn arret_stdlib_println(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> () {
        let mut output = io::stdout();

        for value in values.iter() {
            pprint::pretty_print(&mut output, task, value);
        }

        output.write_all(&[b'\n']).unwrap();
    }
}

define_rust_fn! {
    #[arret_type="(Int ->! (U))"]
    EXIT = fn arret_stdlib_exit(exit_code: i64) -> () {
        use std::process::exit;
        exit(exit_code as i32);
    }
}

define_rust_fn! {
    #[arret_type="(All #{H T} H (Listof T) -> (List H T ...))"]
    CONS = fn arret_stdlib_cons(task: &mut Task, head: Gc<boxed::Any>, tail: Gc<boxed::List<boxed::Any>>) -> Gc<boxed::TopPair> {
        boxed::Pair::new(task, (head, tail)).as_top_pair()
    }
}

define_rust_fn! {
    #[arret_type="(All #{T} T -> T)"]
    BLACK_BOX = fn arret_stdlib_black_box(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
        value
    }
}

define_rust_module! {
    "length" => LENGTH,
    "panic" => PANIC,
    "=" => EQUALS,
    "print!" => PRINT,
    "println!" => PRINTLN,
    "exit" => EXIT,
    "cons" => CONS,
    "black-box" => BLACK_BOX
}
