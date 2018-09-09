#![feature(tool_lints)]
#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use rayon::prelude::*;

use compiler::error::Error;
use compiler::reporting::report_to_stderr;
use compiler::SourceLoader;

use std::alloc::System;
use std::cell::RefCell;
use std::{fs, path};

#[global_allocator]
static GLOBAL: System = System;

thread_local!(static SOURCE_LOADER: RefCell<SourceLoader> = RefCell::new(SourceLoader::new()));

fn try_run_single_test(
    source_loader: &mut SourceLoader,
    input_path: &path::Path,
) -> Result<(), Error> {
    let source_file_id = source_loader.load_path(input_path).unwrap();
    let package_paths = compiler::PackagePaths::test_paths();

    let hir = compiler::lower_program(&package_paths, source_loader, source_file_id)?;
    let inferred_defs = compiler::infer_program(&hir.pvars, &hir.tvars, hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(compiler::EvalHirMode::PureOnly);
    for inferred_def in inferred_defs {
        ehx.consume_def(&hir.tvars, inferred_def)?;
    }

    ehx.eval_main_fun(&hir.tvars, hir.main_var_id)?;

    Ok(())
}

fn run_single_test(input_path: &path::Path) -> bool {
    SOURCE_LOADER.with(|source_loader| {
        use std::io;

        let result = try_run_single_test(&mut *source_loader.borrow_mut(), input_path);

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
fn run_pass() {
    let entries = fs::read_dir("./tests/run-pass").unwrap();

    let failed_tests = entries
        .par_bridge()
        .filter_map(|entry| {
            let input_path = entry.unwrap().path();

            if !run_single_test(input_path.as_path()) {
                Some(input_path.to_string_lossy().to_string())
            } else {
                None
            }
        }).collect::<Vec<String>>();

    if !failed_tests.is_empty() {
        panic!("run-pass tests failed: {}", failed_tests.join(", "))
    }
}
