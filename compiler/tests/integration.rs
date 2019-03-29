#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::env;
use std::ops::Range;

use rayon::prelude::*;
use tempfile::NamedTempFile;

use syntax::span::Span;

use compiler::error::Error;
use compiler::reporting::{report_to_stderr, LocTrace, Reportable, Severity};
use compiler::SourceLoader;

use std::{fs, path, process};

#[derive(Clone, Copy, PartialEq)]
enum TestType {
    CompileFail,
    RunPass,
    EvalPass,
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
    severity: Severity,
    message_prefix: String,
    span: ExpectedSpan,
}

impl ExpectedReport {
    fn matches(&self, actual_report: &dyn Reportable) -> bool {
        let loc_trace = actual_report.loc_trace();
        let candidate_spans = &[loc_trace.origin(), loc_trace.macro_invocation()];

        candidate_spans.iter().any(|candidate_span| {
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
            self.severity().to_str(),
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

fn extract_expected_reports(source_file: &compiler::SourceFile) -> Vec<ExpectedReport> {
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
                severity,
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
            severity,
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
    source_loader: &SourceLoader,
    source_file: &compiler::SourceFile,
    test_type: TestType,
) -> Result<(), Error> {
    let package_paths = compiler::PackagePaths::test_paths(target_triple);

    let hir = compiler::lower_program(&package_paths, source_loader, &source_file)?;
    let inferred_defs = compiler::infer_program(hir.defs, hir.main_var_id)?;

    let mut ehx = compiler::EvalHirCtx::new(true);
    for inferred_def in inferred_defs {
        ehx.consume_def(inferred_def)?;
    }

    // Try evaluating
    ehx.eval_main_fun(hir.main_var_id)?;

    if test_type != TestType::RunPass {
        return Ok(());
    }

    // And now compiling and running
    let mir_program = ehx.into_built_program(hir.main_var_id)?;
    if mir_program.is_empty() {
        return Ok(());
    }

    let output_path = NamedTempFile::new().unwrap().into_temp_path();

    let gen_program_opts =
        compiler::GenProgramOptions::new().with_target_triple(target_triple.as_ref().map(|x| &**x));

    compiler::gen_program(
        gen_program_opts,
        &hir.rust_libraries,
        &mir_program,
        &output_path,
        None,
    );

    let status = process::Command::new(output_path.as_os_str())
        .status()
        .unwrap();

    if !status.success() {
        panic!(
            "unexpected status {} returned from compiled test {}",
            status,
            source_file.kind()
        );
    }

    Ok(())
}

fn run_single_pass_test(
    target_triple: Option<&str>,
    source_loader: &SourceLoader,
    source_file: &compiler::SourceFile,
    test_type: TestType,
) -> bool {
    use std::io;

    let result = result_for_single_test(target_triple, source_loader, &source_file, test_type);

    if let Err(Error(errs)) = result {
        // Prevent concurrent writes to stderr
        let stderr = io::stderr();
        let _errlock = stderr.lock();

        for err in errs {
            report_to_stderr(source_loader, &*err);
        }

        false
    } else {
        true
    }
}

fn run_single_compile_fail_test(
    target_triple: Option<&str>,
    source_loader: &SourceLoader,
    source_file: &compiler::SourceFile,
) -> bool {
    use std::io;

    let result = result_for_single_test(
        target_triple,
        source_loader,
        source_file,
        TestType::CompileFail,
    );

    let mut expected_reports = extract_expected_reports(source_file);
    let actual_reports = if let Err(Error(reports)) = result {
        reports
    } else {
        panic!(
            "Compilation unexpectedly succeeded for {}",
            source_file.kind()
        )
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
        report_to_stderr(&source_loader, unexpected_report.as_ref());
    }

    for expected_report in expected_reports {
        report_to_stderr(&source_loader, &expected_report);
    }

    false
}

fn run_single_test(
    target_triple: Option<&str>,
    source_loader: &SourceLoader,
    input_path: &path::Path,
    test_type: TestType,
) -> bool {
    let source_file = source_loader.load_path(input_path).unwrap();

    if test_type == TestType::CompileFail {
        run_single_compile_fail_test(target_triple, source_loader, &source_file)
    } else {
        run_single_pass_test(target_triple, source_loader, &source_file, test_type)
    }
}

#[test]
fn pass() {
    let target_triple =
        env::var_os("ARRET_TEST_TARGET_TRIPLE").map(|os_str| os_str.into_string().unwrap());

    let source_loader = SourceLoader::new();

    use compiler::initialise_llvm;
    initialise_llvm(target_triple.is_some());

    let compile_fail_entries = fs::read_dir("./tests/compile-fail")
        .unwrap()
        .map(|entry| (entry, TestType::CompileFail));

    let eval_entries = fs::read_dir("./tests/eval-pass")
        .unwrap()
        .chain(fs::read_dir("./tests/optimise").unwrap())
        .map(|entry| (entry, TestType::EvalPass));

    let run_entries = fs::read_dir("./tests/run-pass")
        .unwrap()
        .map(|entry| (entry, TestType::RunPass));

    let failed_tests = compile_fail_entries
        .chain(eval_entries)
        .chain(run_entries)
        .par_bridge()
        .filter_map(|(entry, test_type)| {
            let input_path = entry.unwrap().path();

            if !run_single_test(
                target_triple.as_ref().map(|t| &**t),
                &source_loader,
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
        panic!("pass tests failed: {}", failed_tests.join(", "))
    }
}
