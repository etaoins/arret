#![feature(tool_lints)]
#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use rayon::prelude::*;
use tempfile::NamedTempFile;

use compiler::error::Error;
use compiler::reporting::report_to_stderr;
use compiler::SourceLoader;

use std::alloc::System;
use std::cell::RefCell;
use std::{fs, path, process};

#[global_allocator]
static GLOBAL: System = System;

thread_local!(static SOURCE_LOADER: RefCell<SourceLoader> = RefCell::new(SourceLoader::new()));

#[derive(Clone, Copy, PartialEq)]
enum TestType {
    RunPass,
    EvalPass,
}

fn try_run_single_test(
    source_loader: &mut SourceLoader,
    input_path: &path::Path,
    test_type: TestType,
) -> Result<(), Error> {
    let source_file_id = source_loader.load_path(input_path).unwrap();
    let package_paths = compiler::PackagePaths::test_paths();

    let hir = compiler::lower_program(&package_paths, source_loader, source_file_id)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new();
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    // Try evaluating
    ehx.eval_main_fun(hir.main_var_id)?;

    // And now compiling
    let mir_program = ehx.into_built_program(hir.main_var_id)?;
    if mir_program.is_empty() {
        // This is okay for all test types
        return Ok(());
    }

    if test_type == TestType::EvalPass {
        panic!(
            "unexpectedly built program for test {}",
            input_path.to_string_lossy()
        );
    }

    let output_path = NamedTempFile::new().unwrap().into_temp_path();
    compiler::gen_program(
        &hir.rust_libraries,
        &mir_program,
        None,
        compiler::OutputType::Executable,
        &output_path,
    );

    let status = process::Command::new(output_path.as_os_str())
        .status()
        .unwrap();

    if !status.success() {
        panic!(
            "unexpected status {} returned from compiled test {}",
            status,
            input_path.to_string_lossy(),
        );
    }

    Ok(())
}

fn run_single_test(input_path: &path::Path, test_type: TestType) -> bool {
    SOURCE_LOADER.with(|source_loader| {
        use std::io;

        let result = try_run_single_test(&mut *source_loader.borrow_mut(), input_path, test_type);

        if let Err(Error(errs)) = result {
            // Prevent concurrent writes to stderr
            let stderr = io::stderr();
            let _errlock = stderr.lock();

            for err in errs {
                report_to_stderr(&*source_loader.borrow(), &*err);
            }

            false
        } else {
            true
        }
    })
}

#[test]
fn pass() {
    let eval_entries = fs::read_dir("./tests/eval-pass")
        .unwrap()
        .map(|entry| (entry, TestType::RunPass));

    let run_entries = fs::read_dir("./tests/run-pass")
        .unwrap()
        .map(|entry| (entry, TestType::RunPass));

    let failed_tests = eval_entries
        .chain(run_entries)
        .par_bridge()
        .filter_map(|(entry, test_type)| {
            let input_path = entry.unwrap().path();

            if !run_single_test(input_path.as_path(), test_type) {
                Some(input_path.to_string_lossy().to_string())
            } else {
                None
            }
        })
        .collect::<Vec<String>>();

    if !failed_tests.is_empty() {
        panic!("pass tests failed: {}", failed_tests.join(", "))
    }
}
