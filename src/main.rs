extern crate clap;

mod syntax;
mod ty;
mod hir;
mod ctx;

use std::fs::File;
use hir::lowering::LoweringContext;
use clap::{App, Arg};
use ctx::CompileContext;

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
    let mut lcx = LoweringContext::new(&mut ccx);
    let exprs = lcx.lower_program(input_path.to_owned(), &mut input_file)
        .unwrap();

    println!("{:?}", exprs);
}
