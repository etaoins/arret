use std::io;
use std::io::prelude::*;

use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

use crate::pretty_print::pretty_print;

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_print(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let mut output = io::stdout();

    for value in values.iter() {
        pretty_print(&mut output, task, value);
    }
}

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_println(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let mut output = io::stdout();

    for value in values.iter() {
        pretty_print(&mut output, task, value);
    }

    output.write_all(&[b'\n']).unwrap();
}
