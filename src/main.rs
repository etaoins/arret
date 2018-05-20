#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate ansi_term;
extern crate clap;

mod ctx;
mod hir;
mod reporting;
mod syntax;
mod ty;
mod typeck;

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

    let hir = match hir::lowering::lower_program(&mut ccx, input_path.to_owned(), &mut input_file) {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                err.report(&ccx);
            }
            return;
        }
    };

    match typeck::infer::infer_program(hir.pvars(), hir.tvars(), hir.defs()) {
        Ok(()) => {
            println!("{:?}", hir.defs());
        }
        Err(errs) => {
            for err in errs {
                err.report(&ccx);
            }
            return;
        }
    }
}
