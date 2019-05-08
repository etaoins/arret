#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::ops::Range;
use std::{env, fs, io, path, process};

use rayon::prelude::*;
use tempfile::NamedTempFile;

use arret_syntax::span::Span;

use arret_compiler::error::Error;
use arret_compiler::reporting::{report_to_stderr, LocTrace, Reportable, Severity};
use arret_compiler::CompileCtx;

#[derive(Clone, Copy, PartialEq)]
enum RunType {
    Pass,
    Error,
}

#[derive(Clone, Copy, PartialEq)]
enum TestType {
    CompileError,
    Optimise,
    Run(RunType),
}

#[derive(Debug)]
enum ExpectedSpan {
    Exact(Span),
    StartRange(Range<usize>),
}

impl ExpectedSpan {
    fn matches(&self, actual_span: Span) -> bool {
        match self {
            ExpectedSpan::Exact(span) => *span == actual_span,
            ExpectedSpan::StartRange(span_range) => {
                let actual_span_start = actual_span.start() as usize;
                actual_span_start >= span_range.start && actual_span_start < span_range.end
            }
        }
    }
}

#[derive(Debug)]
struct ExpectedReport {
    expected_severity: Severity,
    message_prefix: String,
    span: ExpectedSpan,
}

impl ExpectedReport {
    fn matches(&self, actual_report: &dyn Reportable) -> bool {
        use std::iter;

        let loc_trace = actual_report.loc_trace();

        iter::once(&loc_trace.origin())
            .chain(loc_trace.macro_invocation().iter())
            .any(|candidate_span| {
                self.span.matches(*candidate_span)
                    && actual_report
                        .message()
                        .starts_with(&self.message_prefix[..])
            })
    }
}

impl Reportable for ExpectedReport {
    fn loc_trace(&self) -> LocTrace {
        (match self.span {
            ExpectedSpan::Exact(span) => span,
            ExpectedSpan::StartRange(ref span_range) => {
                Span::new(span_range.start as u32, span_range.end as u32)
            }
        })
        .into()
    }

    fn severity(&self) -> Severity {
        Severity::Help
    }

    fn message(&self) -> String {
        format!(
            "expected {} `{} ...`",
            self.expected_severity.to_str(),
            self.message_prefix
        )
    }
}

fn take_severity(marker_string: &str) -> (Severity, &str) {
    for (prefix, severity) in &[
        (" ERROR ", Severity::Error),
        (" NOTE ", Severity::Note),
        (" HELP ", Severity::Help),
    ] {
        if marker_string.starts_with(prefix) {
            return (*severity, &marker_string[prefix.len()..]);
        }
    }

    panic!("Unknown severity prefix for `{}`", marker_string)
}

fn extract_expected_reports(source_file: &arret_compiler::SourceFile) -> Vec<ExpectedReport> {
    let source = source_file.source();
    let span_offset = source_file.span().start() as usize;

    let mut line_reports = source
        .match_indices(";~")
        .map(|(index, _)| {
            let start_of_line_index = &source[..index].rfind('\n').unwrap_or(0);

            let end_of_line_index = &source[index..]
                .find('\n')
                .map(|i| i + index)
                .unwrap_or_else(|| source.len());

            // Take from after the ;~ to the end of the line
            let marker_string = &source[index + 2..*end_of_line_index];
            let (severity, marker_string) = take_severity(marker_string);

            ExpectedReport {
                expected_severity: severity,
                message_prefix: marker_string.into(),
                span: ExpectedSpan::StartRange(
                    span_offset + start_of_line_index..span_offset + index,
                ),
            }
        })
        .collect::<Vec<ExpectedReport>>();

    let mut spanned_reports = source.match_indices(";^").map(|(index, _)| {
        let span_length = source[index..].find(' ').expect("Cannot find severity") - 1;

        let start_of_line_index = &source[..index]
            .rfind('\n')
            .expect("Cannot have a spanned error on first line");

        let start_of_previous_line_index = &source[..*start_of_line_index].rfind('\n').unwrap_or(0);

        let end_of_line_index = &source[index..]
            .find('\n')
            .map(|i| i + index)
            .unwrap_or_else(|| source.len());

        let span_line_offset = index - start_of_line_index + 1;

        let span_start = start_of_previous_line_index + span_line_offset;
        let span_end = span_start + span_length;

        // Take from after the ;^^ to the end of the line
        let marker_string = &source[index + span_length + 1..*end_of_line_index];
        let (severity, marker_string) = take_severity(marker_string);

        ExpectedReport {
            expected_severity: severity,
            message_prefix: marker_string.into(),
            span: ExpectedSpan::Exact(Span::new(
                (span_offset + span_start) as u32,
                (span_offset + span_end) as u32,
            )),
        }
    });

    line_reports.extend(&mut spanned_reports);
    line_reports
}

fn result_for_single_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> Result<(), Error> {
    let hir = arret_compiler::lower_program(ccx, &source_file)?;
    let inferred_defs = arret_compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = arret_compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    // Try evaluating if we're not supposed to panic
    if test_type != TestType::Run(RunType::Error) {
        ehx.eval_main_fun(hir.main_var_id)?;
    }

    let run_type = if let TestType::Run(run_type) = test_type {
        run_type
    } else {
        return Ok(());
    };

    // And now compiling and running
    let mir_program = ehx.into_built_program(hir.main_var_id)?;

    if mir_program.is_empty() {
        // Don't bother building
        return Ok(());
    }

    let output_path = NamedTempFile::new().unwrap().into_temp_path();

    let gen_program_opts = arret_compiler::GenProgramOptions::new()
        .with_target_triple(target_triple.as_ref().map(|x| &**x));

    arret_compiler::gen_program(
        gen_program_opts,
        &hir.rust_libraries,
        &mir_program,
        &output_path,
        None,
    );

    let mut process = process::Command::new(output_path.as_os_str());

    match run_type {
        RunType::Pass => {
            let status = process.status().unwrap();
            if !status.success() {
                panic!(
                    "unexpected status {} returned from integration test {}",
                    status,
                    source_file.kind()
                );
            }
        }
        RunType::Error => {
            // Discard our panic output
            let status = process.stderr(process::Stdio::null()).status().unwrap();

            // Code 1 is used by panic. This makes sure we didn't e.g. SIGSEGV.
            if status.code() != Some(1) {
                panic!(
                    "unexpected status {} returned from integration test {}",
                    status,
                    source_file.kind()
                );
            }
        }
    }

    Ok(())
}

fn run_single_pass_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> bool {
    use std::io;

    let result = result_for_single_test(target_triple, ccx, &source_file, test_type);

    if let Err(Error(errs)) = result {
        // Prevent concurrent writes to stderr
        let stderr = io::stderr();
        let _errlock = stderr.lock();

        for err in errs {
            report_to_stderr(ccx.source_loader(), &*err);
        }

        false
    } else {
        true
    }
}

fn run_single_compile_fail_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
) -> bool {
    use std::io;

    let result = result_for_single_test(target_triple, ccx, source_file, TestType::CompileError);

    let mut expected_reports = extract_expected_reports(source_file);
    let actual_reports = if let Err(Error(reports)) = result {
        reports
    } else {
        eprintln!(
            "Compilation unexpectedly succeeded for {}",
            source_file.kind()
        );
        return false;
    };

    let mut unexpected_reports = vec![];

    for actual_report in actual_reports.into_iter() {
        let expected_report_index = expected_reports
            .iter()
            .position(|expected_report| expected_report.matches(actual_report.as_ref()));

        match expected_report_index {
            Some(index) => {
                expected_reports.swap_remove(index);
            }
            None => {
                unexpected_reports.push(actual_report);
            }
        }
    }

    if unexpected_reports.is_empty() && expected_reports.is_empty() {
        return true;
    }

    // Prevent concurrent writes to stderr
    let stderr = io::stderr();
    let _errlock = stderr.lock();

    for unexpected_report in unexpected_reports {
        eprintln!("Unexpected {}:", unexpected_report.severity().to_str());
        report_to_stderr(ccx.source_loader(), unexpected_report.as_ref());
    }

    for expected_report in expected_reports {
        report_to_stderr(ccx.source_loader(), &expected_report);
    }

    false
}

fn run_single_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    input_path: &path::Path,
    test_type: TestType,
) -> bool {
    let source_file = ccx.source_loader().load_path_uncached(input_path).unwrap();

    if test_type == TestType::CompileError {
        run_single_compile_fail_test(target_triple, ccx, &source_file)
    } else {
        run_single_pass_test(target_triple, ccx, &source_file, test_type)
    }
}

fn entry_to_test_tuple(
    entry: io::Result<fs::DirEntry>,
    test_type: TestType,
) -> Option<(path::PathBuf, TestType)> {
    let entry = entry.unwrap();

    if !entry
        .file_name()
        .to_str()
        .map(|file_name| !file_name.starts_with('.') && file_name.ends_with(".arret"))
        .unwrap_or(false)
    {
        return None;
    }

    Some((entry.path(), test_type))
}

#[test]
fn integration() {
    let target_triple =
        env::var_os("ARRET_TEST_TARGET_TRIPLE").map(|os_str| os_str.into_string().unwrap());

    let package_paths =
        arret_compiler::PackagePaths::test_paths(target_triple.as_ref().map(|t| &**t));
    let ccx = arret_compiler::CompileCtx::new(package_paths, true);

    use arret_compiler::initialise_llvm;
    initialise_llvm(target_triple.is_some());

    let compile_error_entries = fs::read_dir("./tests/compile-error")
        .unwrap()
        .filter_map(|entry| entry_to_test_tuple(entry, TestType::CompileError));

    let optimise_entries = fs::read_dir("./tests/optimise")
        .unwrap()
        .filter_map(|entry| entry_to_test_tuple(entry, TestType::Optimise));

    let run_pass_entries = fs::read_dir("./tests/run-pass")
        .unwrap()
        .filter_map(|entry| entry_to_test_tuple(entry, TestType::Run(RunType::Pass)));

    let run_error_entries = fs::read_dir("./tests/run-error")
        .unwrap()
        .filter_map(|entry| entry_to_test_tuple(entry, TestType::Run(RunType::Error)));

    let failed_tests = compile_error_entries
        .chain(optimise_entries)
        .chain(run_pass_entries)
        .chain(run_error_entries)
        .par_bridge()
        .filter_map(|(input_path, test_type)| {
            if !run_single_test(
                target_triple.as_ref().map(|t| &**t),
                &ccx,
                input_path.as_path(),
                test_type,
            ) {
                Some(input_path.to_string_lossy().to_string())
            } else {
                None
            }
        })
        .collect::<Vec<String>>();

    if !failed_tests.is_empty() {
        panic!("integration tests failed: {}", failed_tests.join(", "))
    }
}
