#![feature(tool_lints)]
#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

mod mode;

use std::{env, path};

use clap::{App, Arg};

pub struct DriverConfig {
    package_paths: compiler::PackagePaths,
}

fn find_path_to_arret_root() -> path::PathBuf {
    let current_dir = env::current_dir().expect("Cannot determine current directory");

    for candidate in path::Path::new(&current_dir).ancestors() {
        if candidate.join("./.arret-root").is_file() {
            return candidate.to_owned();
        }
    }

    panic!("Unable to find the Arret root directory");
}

fn main() {
    let matches = App::new("arret")
        .arg(Arg::with_name("INPUT").help("Input source file").index(1))
        .get_matches();

    let arret_target_dir = find_path_to_arret_root();
    let cfg = DriverConfig {
        package_paths: compiler::PackagePaths::with_stdlib(&arret_target_dir),
    };

    match matches.value_of("INPUT") {
        Some(input_param) => mode::compile::compile_input_file(&cfg, path::Path::new(input_param)),
        None => mode::repl::interactive_loop(&cfg),
    }
}
