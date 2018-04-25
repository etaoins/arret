#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate ansi_term;
extern crate clap;

mod ctx;
mod hir;
mod reporting;
mod syntax;
mod ty;

use clap::{App, Arg};
use ctx::CompileContext;
use reporting::Reportable;
use std::fs::File;

fn main() {
    let matches = App::new("risp")
        .arg(
            Arg::with_name("INPUT")
                .help("Input source file")
                .required(true)
                .index(1),
        )
        .get_matches();

    let input_path = matches.value_of("INPUT").unwrap();
    let mut input_file = File::open(input_path).expect("Unable to open input file");

    let mut ccx = CompileContext::new();

    let result = { hir::lowering::lower_program(&mut ccx, input_path.to_owned(), &mut input_file) };

    match result {
        Ok(program) => {
            println!("{:?}", program.entry_module());
        }
        Err(err) => {
            let _: &reporting::Reportable = &err;
            err.report(&ccx);
        }
    }
}
