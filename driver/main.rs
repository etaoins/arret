#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

mod subcommand;

use std::{env, path, process};

use compiler::CompileCtx;

const ARRET_FILE_EXTENSION: &str = ".arret";

fn find_path_to_arret_root() -> path::PathBuf {
    let current_dir = env::current_dir().expect("Cannot determine current directory");

    for candidate in path::Path::new(&current_dir).ancestors() {
        if candidate.join("./.arret-root").is_file() {
            return candidate.to_owned();
        }
    }

    panic!("Unable to find the Arret root directory");
}

fn input_arg_to_source_file(
    source_loader: &compiler::SourceLoader,
    input_param: &str,
) -> compiler::ArcId<compiler::SourceFile> {
    if input_param == "-" {
        use std::io::prelude::*;

        let mut input_string = String::new();
        std::io::stdin().read_to_string(&mut input_string).unwrap();

        source_loader.load_string(
            compiler::SourceKind::File("stdin".to_owned()),
            input_string.into(),
        )
    } else {
        let input_path = path::Path::new(input_param);

        source_loader
            .load_path_uncached(input_path)
            .expect("Unable to read input file")
    }
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
                .arg(
                    Arg::with_name("INPUT")
                        .required(true)
                        .help("Input source file")
                        .index(1),
                )
                .arg(
                    Arg::with_name("OUTPUT")
                        .short("o")
                        .value_name("FILE")
                        .help("Output filename"),
                )
                .arg(
                    Arg::with_name("DEBUG")
                        .short("g")
                        .long("debug-info")
                        .help("Generates debugging information"),
                )
                .arg(
                    Arg::with_name("TARGET")
                        .long("target")
                        .value_name("TRIPLE")
                        .help("Generate code for the given target"),
                ),
        )
        .subcommand(
            SubCommand::with_name("eval")
                .about("Evaluates an Arret program once")
                .arg(
                    Arg::with_name("INPUT")
                        .required(true)
                        .help("Input source file")
                        .index(1),
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
    let enable_optimisations = !matches.is_present("NOOPT");

    if let Some(compile_matches) = matches.subcommand_matches("compile") {
        let package_paths = compiler::PackagePaths::with_stdlib(
            &arret_target_dir,
            compile_matches.value_of("TARGET"),
        );

        let ccx = CompileCtx::new(package_paths, enable_optimisations);

        let input_arg = compile_matches.value_of("INPUT").unwrap();
        let input_file = input_arg_to_source_file(ccx.source_loader(), input_arg);

        let output_path = path::Path::new(
            if let Some(output_param) = compile_matches.value_of("OUTPUT") {
                output_param
            } else if input_arg.ends_with(ARRET_FILE_EXTENSION) {
                &input_arg[0..input_arg.len() - ARRET_FILE_EXTENSION.len()]
            } else {
                panic!(
                    "Can't determine output filename from input arg `{}`",
                    input_arg
                );
            },
        );

        let debug_info = compile_matches.is_present("DEBUG");

        let target_triple = compile_matches.value_of("TARGET");
        initialise_llvm(target_triple.is_some());

        if !subcommand::compile::compile_input_file(
            &ccx,
            &input_file,
            target_triple,
            &output_path,
            debug_info,
        ) {
            process::exit(2);
        }
    } else if let Some(repl_matches) = matches.subcommand_matches("repl") {
        let package_paths = compiler::PackagePaths::with_stdlib(&arret_target_dir, None);
        let ccx = CompileCtx::new(package_paths, enable_optimisations);

        initialise_llvm(false);

        let include_path = repl_matches
            .value_of("INCLUDE")
            .map(|include_param| path::Path::new(include_param).to_owned());

        subcommand::repl::interactive_loop(&ccx, include_path);
    } else if let Some(eval_matches) = matches.subcommand_matches("eval") {
        let package_paths = compiler::PackagePaths::with_stdlib(&arret_target_dir, None);
        let ccx = CompileCtx::new(package_paths, enable_optimisations);

        let input_param = eval_matches.value_of("INPUT").unwrap();
        let input_file = input_arg_to_source_file(ccx.source_loader(), input_param);

        initialise_llvm(false);

        if !subcommand::eval::eval_input_file(&ccx, &input_file) {
            process::exit(2);
        }
    } else {
        eprintln!("Sub-command not specified");
        process::exit(1);
    }
}
