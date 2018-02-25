//#![feature(nll)]

extern crate ansi_term;
extern crate clap;

mod syntax;
mod ty;
mod hir;
mod ctx;
mod reporting;

use std::fs::File;
use hir::lowering::LoweringContext;
use clap::{App, Arg};
use ctx::CompileContext;
use reporting::Reportable;

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

    let result = {
        let mut lcx = LoweringContext::new(&mut ccx);
        lcx.lower_program(input_path.to_owned(), &mut input_file)
    };

    match result {
        Ok(exprs) => {
            println!("{:?}", exprs);
        }
        Err(err) => {
            let _: &reporting::Reportable = &err;
            err.report(&ccx);
        }
    }
}
