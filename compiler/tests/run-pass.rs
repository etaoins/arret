#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]

extern crate compiler;

use compiler::reporting::Reportable;
use std::{fs, path};

fn run_single_test(source_loader: &mut compiler::SourceLoader, input_path: &path::Path) -> bool {
    let source_file_id = source_loader.load_path(input_path).unwrap();
    let package_paths = compiler::PackagePaths::default();

    let hir = match compiler::lower_program(&package_paths, source_loader, source_file_id) {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                err.report(source_loader);
            }
            return false;
        }
    };

    match compiler::infer_program(&hir.pvars, &hir.tvars, hir.module_defs) {
        Ok(_) => {}
        Err(errs) => {
            for err in errs {
                err.report(source_loader);
            }
            return false;
        }
    }

    true
}

#[test]
fn run_pass() {
    let entries = fs::read_dir("./tests/run-pass").unwrap();
    let mut failed_tests = vec![];

    let mut source_loader = compiler::SourceLoader::new();
    for entry in entries {
        let input_path = entry.unwrap().path();

        if !run_single_test(&mut source_loader, input_path.as_path()) {
            failed_tests.push(input_path.to_string_lossy().to_string());
        }
    }

    if !failed_tests.is_empty() {
        panic!("run-pass tests failed: {}", failed_tests.join(", "))
    }
}
