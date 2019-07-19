use std::io;
use std::io::prelude::*;

use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

fn pretty_print_common(
    task: &mut Task,
    values: Gc<boxed::List<boxed::Any>>,
    output: &mut dyn Write,
) {
    for value in values.iter() {
        crate::pretty_print::pretty_print(output, task, value);
    }
}

fn write_boxed_common(
    task: &mut Task,
    values: Gc<boxed::List<boxed::Any>>,
    output: &mut dyn Write,
) {
    let mut is_first = true;
    for value in values.iter() {
        if !is_first {
            output.write_all(&[b' ']).unwrap();
        }

        arret_runtime_syntax::writer::write_boxed(output, task, value).unwrap();
        is_first = false;
    }
}

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_print(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let stdout = io::stdout();
    let mut output = stdout.lock();

    pretty_print_common(task, values, &mut output);
}

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_println(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let stdout = io::stdout();
    let mut output = stdout.lock();

    pretty_print_common(task, values, &mut output);
    output.write_all(&[b'\n']).unwrap();
}

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_write(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let stdout = io::stdout();
    let mut output = stdout.lock();

    write_boxed_common(task, values, &mut output);
}

#[arret_rfi_derive::rust_fun("(& Any ->! ())")]
pub fn stdlib_writeln(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) {
    let stdout = io::stdout();
    let mut output = stdout.lock();

    write_boxed_common(task, values, &mut output);
    output.write_all(&[b'\n']).unwrap();
}
