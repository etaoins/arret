use std::io;
use std::io::prelude::*;

use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

use rfi_derive;

use crate::pretty_print::pretty_print;

#[rfi_derive::rust_fun("(Any ... ->! ())")]
pub fn stdlib_print(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let mut output = io::stdout();

    for value in values.iter() {
        pretty_print(&mut output, task, value);
    }
}

#[rfi_derive::rust_fun("(Any ... ->! ())")]
pub fn stdlib_println(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let mut output = io::stdout();

    for value in values.iter() {
        pretty_print(&mut output, task, value);
    }

    output.write_all(&[b'\n']).unwrap();
}
