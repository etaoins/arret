#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

extern crate clap;
extern crate compiler;
extern crate syntax;

use std::path;

use clap::{App, Arg};
use compiler::reporting::Reportable;

fn main() {
    let matches = App::new("arret")
        .arg(
            Arg::with_name("INPUT")
                .help("Input source file")
                .required(true)
                .index(1),
        )
        .get_matches();

    let input_path = path::Path::new(matches.value_of("INPUT").unwrap());

    let package_paths = compiler::PackagePaths::default();

    let mut source_loader = compiler::SourceLoader::new();
    let source_file_id = source_loader
        .load_path(input_path)
        .expect("Unable to read input file");

    let hir = match compiler::lower_program(&package_paths, &mut source_loader, source_file_id) {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                err.report(&source_loader);
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
                err.report(&source_loader);
            }
            return;
        }
    }
}
