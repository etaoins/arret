#![feature(tool_lints)]
#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use compiler::error::Error;
use compiler::reporting::report_to_stderr;

use std::{fs, path};

fn try_run_single_test(
    source_loader: &mut compiler::SourceLoader,
    input_path: &path::Path,
) -> Result<(), Error> {
    let source_file_id = source_loader.load_path(input_path).unwrap();
    let package_paths = compiler::PackagePaths::test_paths();

    let hir = compiler::lower_program(&package_paths, source_loader, source_file_id)?;
    let inferred_defs = compiler::infer_program(&hir.pvars, &hir.tvars, hir.defs, hir.main_var_id)?;

    let mut pcx = compiler::PartialEvalCtx::new();
    for inferred_def in inferred_defs {
        pcx.consume_def(&hir.tvars, inferred_def)?;
    }

    Ok(())
}

fn run_single_test(source_loader: &mut compiler::SourceLoader, input_path: &path::Path) -> bool {
    let result = try_run_single_test(source_loader, input_path);

    if let Err(Error(errs)) = result {
        for err in errs {
            report_to_stderr(source_loader, &*err);
        }
        false
    } else {
        true
    }
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
