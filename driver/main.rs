#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

mod mode;

use std::alloc::System;
use std::{env, path, process};

use clap::{App, Arg};

#[global_allocator]
static GLOBAL: System = System;

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
        .arg(
            Arg::with_name("OUTPUT")
                .short("o")
                .value_name("FILE")
                .help("Output filename"),
        )
        .arg(
            Arg::with_name("TARGET")
                .long("target")
                .value_name("TRIPLE")
                .help("Generate code for the given target"),
        )
        .get_matches();

    let arret_target_dir = find_path_to_arret_root();

    let cfg = DriverConfig {
        package_paths: compiler::PackagePaths::with_stdlib(
            &arret_target_dir,
            matches.value_of("TARGET"),
        ),
    };

    match matches.value_of("INPUT") {
        Some(input_param) => {
            let input_path = path::Path::new(input_param);

            let output_path = match matches.value_of("OUTPUT") {
                Some(output_param) => path::Path::new(output_param).to_owned(),
                None => input_path.with_extension(""),
            };

            let target_triple = matches.value_of("TARGET");

            if !mode::compile::compile_input_file(&cfg, input_path, target_triple, &output_path) {
                process::exit(2);
            }
        }
        None => {
            if matches.value_of("OUTPUT").is_some() {
                eprintln!("-o cannot be used with REPL");
                process::exit(1);
            }

            if matches.value_of("TARGET").is_some() {
                eprintln!("--target cannot be used with REPL");
                process::exit(1);
            }

            mode::repl::interactive_loop(&cfg);
        }
    };
}
