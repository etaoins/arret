extern crate compiler;

use compiler::reporting::Reportable;
use std::{fs, path};

fn run_single_test(display_name: String, input_path: path::PathBuf) -> bool {
    let source = fs::read_to_string(input_path).unwrap();

    let mut ccx = compiler::CompileContext::new();

    let hir = match compiler::lower_program(&mut ccx, display_name, source) {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                err.report(&ccx);
            }
            return false;
        }
    };

    match compiler::infer_program(&hir.pvars, &hir.tvars, hir.defs) {
        Ok(_) => {}
        Err(errs) => {
            for err in errs {
                err.report(&ccx);
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

    for entry in entries {
        let input_path = entry.unwrap().path();
        let display_name = input_path.to_string_lossy().to_string();

        if !run_single_test(display_name.clone(), input_path) {
            failed_tests.push(display_name);
        }
    }

    if !failed_tests.is_empty() {
        panic!("run-pass tests failed: {}", failed_tests.join(", "))
    }
}
