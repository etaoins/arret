#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate clap;
extern crate compiler;
extern crate syntax;

use clap::{App, Arg};
use compiler::reporting::Reportable;
use std::fs;

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
    let source = fs::read_to_string(input_path).expect("Unable to read input file");

    let mut ccx = compiler::CompileContext::new();

    let hir = match compiler::lower_program(&mut ccx, input_path.into(), source) {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                err.report(&ccx);
            }
            return;
        }
    };

    match compiler::infer_program(&hir.pvars, &hir.tvars, hir.defs) {
        Ok(inferred_defs) => {
            for inferred_def in inferred_defs {
                println!("{:?}", inferred_def);
            }
        }
        Err(errs) => {
            for err in errs {
                err.report(&ccx);
            }
            return;
        }
    }
}
