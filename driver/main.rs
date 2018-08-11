#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]

extern crate ansi_term;
extern crate app_dirs;
extern crate clap;
extern crate compiler;
extern crate rustyline;
extern crate syntax;

mod mode;

use std::path;

use clap::{App, Arg};

pub struct DriverConfig {
    package_paths: compiler::PackagePaths,
}

fn main() {
    let matches = App::new("arret")
        .arg(Arg::with_name("INPUT").help("Input source file").index(1))
        .get_matches();

    let cfg = DriverConfig {
        package_paths: compiler::PackagePaths::default(),
    };

    match matches.value_of("INPUT") {
        Some(input_param) => mode::compile::compile_input_file(&cfg, path::Path::new(input_param)),
        None => mode::repl::interactive_loop(&cfg),
    }
}
