use std::io;
use std::io::prelude::*;

use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

use crate::pretty_print::pretty_print;

define_rust_fn! {
    #[arret_type="(Any ... ->! ())"]
    pub PRINT = fn arret_stdlib_print(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> () {
        let mut output = io::stdout();

        for value in values.iter() {
            pretty_print(&mut output, task, value);
        }
    }
}

define_rust_fn! {
    #[arret_type="(Any ... ->! ())"]
    pub PRINTLN = fn arret_stdlib_println(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> () {
        let mut output = io::stdout();

        for value in values.iter() {
            pretty_print(&mut output, task, value);
        }

        output.write_all(&[b'\n']).unwrap();
    }
}
