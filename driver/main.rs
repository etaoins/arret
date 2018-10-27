#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

mod subcommand;

use std::alloc::System;
use std::{env, path, process};

#[global_allocator]
static GLOBAL: System = System;

pub struct DriverConfig {
    package_paths: compiler::PackagePaths,
    llvm_opt: bool,
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
    use clap::{crate_version, App, AppSettings, Arg, SubCommand};
    use compiler::initialise_llvm;

    let matches = App::new("arret")
        .version(crate_version!())
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .about("Compiler and REPL for the Arret language")
        .arg(
            Arg::with_name("NOOPT")
                .long("no-llvm-opt")
                .takes_value(false)
                .help("Disable LLVM optimisation"),
        )
        .subcommand(
            SubCommand::with_name("compile")
                .about("Compiles an Arret program to a standalone binary")
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
                ),
        )
        .subcommand(
            SubCommand::with_name("repl")
                .about("Starts an interactive REPL")
                .arg(
                    Arg::with_name("INCLUDE")
                        .short("i")
                        .long("include")
                        .value_name("FILE")
                        .help("file to preload before starting REPL"),
                ),
        )
        .get_matches();

    let arret_target_dir = find_path_to_arret_root();

    let cfg = DriverConfig {
        package_paths: compiler::PackagePaths::with_stdlib(
            &arret_target_dir,
            matches.value_of("TARGET"),
        ),
        llvm_opt: !matches.is_present("NOOPT"),
    };

    if let Some(compile_matches) = matches.subcommand_matches("compile") {
        let input_param = compile_matches.value_of("INPUT").unwrap();

        let input_path = path::Path::new(input_param);

        let output_path = match compile_matches.value_of("OUTPUT") {
            Some(output_param) => path::Path::new(output_param).to_owned(),
            None => input_path.with_extension(""),
        };

        let target_triple = compile_matches.value_of("TARGET");
        initialise_llvm(target_triple.is_some());

        if !subcommand::compile::compile_input_file(&cfg, input_path, target_triple, &output_path) {
            process::exit(2);
        }
    } else if let Some(repl_matches) = matches.subcommand_matches("repl") {
        initialise_llvm(false);

        let include_path = repl_matches
            .value_of("INCLUDE")
            .map(|include_param| path::Path::new(include_param).to_owned());

        subcommand::repl::interactive_loop(&cfg, include_path);
    } else {
        eprintln!("Sub-command not specified");
        process::exit(1);
    }
}
