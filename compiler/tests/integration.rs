#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::ops::Range;
use std::{env, fs, io, path, process};

use codespan_reporting::{Diagnostic, Label, LabelStyle, Severity};
use rayon::prelude::*;
use tempfile::NamedTempFile;

use arret_syntax::span::{ByteIndex, Span};

use arret_compiler::{emit_diagnostics_to_stderr, errors_to_diagnostics, CompileCtx, OutputType};

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
    StartRange(Range<ByteIndex>),
}

impl ExpectedSpan {
    fn matches(&self, actual_span: Span) -> bool {
        match self {
            ExpectedSpan::Exact(span) => *span == actual_span,
            ExpectedSpan::StartRange(span_range) => {
                let actual_span_start = actual_span.start();
                actual_span_start >= span_range.start && actual_span_start < span_range.end
            }
        }
    }
}

#[derive(Debug)]
struct ExpectedDiagnostic {
    expected_severity: Severity,
    message_prefix: String,
    span: ExpectedSpan,
}

impl ExpectedDiagnostic {
    fn matches(&self, actual: &Diagnostic) -> bool {
        if self.expected_severity != actual.severity {
            return false;
        }

        if !actual.message.starts_with(&self.message_prefix[..]) {
            return false;
        }

        actual
            .labels
            .iter()
            .any(|candidate_label| self.span.matches(candidate_label.span))
    }

    /// Returns a diagnostic for reporting missing expectation
    fn to_error_diagnostic(&self) -> Diagnostic {
        let span = match self.span {
            ExpectedSpan::Exact(span) => span,
            ExpectedSpan::StartRange(ref span_range) => Span::new(span_range.start, span_range.end),
        };

        Diagnostic::new_error(format!("expected {}", self.expected_severity.to_str(),)).with_label(
            Label::new_primary(span).with_message(format!("{} ...", self.message_prefix)),
        )
    }
}

fn take_severity(marker_string: &str) -> (Severity, &str) {
    for (prefix, severity) in &[
        (" ERROR ", Severity::Error),
        (" WARNING ", Severity::Warning),
        (" NOTE ", Severity::Note),
        (" HELP ", Severity::Help),
    ] {
        if marker_string.starts_with(prefix) {
            return (*severity, &marker_string[prefix.len()..]);
        }
    }

    panic!("Unknown severity prefix for `{}`", marker_string)
}

fn extract_expected_diagnostics(
    source_file: &arret_compiler::SourceFile,
) -> Vec<ExpectedDiagnostic> {
    let file_map = source_file.file_map();

    let source = file_map.src();
    let span_offset = file_map.span().start().0;

    source
        .match_indices(";~")
        .map(|(index, _)| {
            let start_of_line_index = &source[..index].rfind('\n').map(|i| i + 1).unwrap_or(0);

            let end_of_line_index = &source[index..]
                .find('\n')
                .map(|i| i + index)
                .unwrap_or_else(|| source.len());

            // Take from after the ;~ to the end of the line
            let marker_string = &source[index + 2..*end_of_line_index];
            let (severity, marker_string) = take_severity(marker_string);

            ExpectedDiagnostic {
                expected_severity: severity,
                message_prefix: marker_string.into(),
                span: ExpectedSpan::StartRange(
                    ByteIndex(span_offset + (*start_of_line_index as u32))
                        ..ByteIndex(span_offset + (index as u32)),
                ),
            }
        })
        .chain(source.match_indices(";^").map(|(index, _)| {
            let span_length = source[index..].find(' ').expect("Cannot find severity") - 1;

            let start_of_line_index = &source[..index]
                .rfind('\n')
                .expect("Cannot have a spanned error on first line");

            let start_of_previous_line_index = &source[..*start_of_line_index]
                .rfind('\n')
                .map(|i| i + 1)
                .unwrap_or(0);

            let end_of_line_index = &source[index..]
                .find('\n')
                .map(|i| i + index)
                .unwrap_or_else(|| source.len());

            let span_line_offset = index - start_of_line_index;

            let span_start = start_of_previous_line_index + span_line_offset;
            let span_end = span_start + span_length;

            // Take from after the ;^^ to the end of the line
            let marker_string = &source[index + span_length + 1..*end_of_line_index];
            let (severity, marker_string) = take_severity(marker_string);

            ExpectedDiagnostic {
                expected_severity: severity,
                message_prefix: marker_string.into(),
                span: ExpectedSpan::Exact(Span::new(
                    ByteIndex(span_offset + (span_start as u32)),
                    ByteIndex(span_offset + (span_end as u32)),
                )),
            }
        }))
        .collect()
}

fn result_for_single_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> Result<(), Vec<Diagnostic>> {
    let skip_run_executable = env::var_os("ARRET_TEST_SKIP_RUN_EXECUTABLE").is_some();
    let hir = arret_compiler::lower_program(ccx, &source_file).map_err(errors_to_diagnostics)?;

    let inferred_defs =
        arret_compiler::infer_program(hir.defs, hir.main_var_id).map_err(errors_to_diagnostics)?;

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

    let gen_program_opts = if skip_run_executable {
        gen_program_opts
            .with_output_type(OutputType::None)
            .with_llvm_opt(false)
    } else {
        gen_program_opts
    };

    arret_compiler::gen_program(
        gen_program_opts,
        &hir.rust_libraries,
        &mir_program,
        &output_path,
        None,
    );

    if skip_run_executable {
        return Ok(());
    }

    let mut process = process::Command::new(output_path.as_os_str());

    match run_type {
        RunType::Pass => {
            let status = process.status().unwrap();
            if !status.success() {
                panic!(
                    "unexpected status {} returned from integration test {}",
                    status,
                    source_file.file_map().name()
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
                    source_file.file_map().name()
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
    let result = result_for_single_test(target_triple, ccx, &source_file, test_type);

    if let Err(diagnostics) = result {
        emit_diagnostics_to_stderr(ccx.source_loader(), diagnostics);
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
    let result = result_for_single_test(target_triple, ccx, source_file, TestType::CompileError);

    let mut expected_diags = extract_expected_diagnostics(source_file);
    let actual_diags = if let Err(diags) = result {
        diags
    } else {
        eprintln!(
            "Compilation unexpectedly succeeded for {}",
            source_file.file_map().name()
        );
        return false;
    };

    let mut unexpected_diags = vec![];

    for actual_diag in actual_diags.into_iter() {
        let expected_report_index = expected_diags
            .iter()
            .position(|expected_report| expected_report.matches(&actual_diag));

        match expected_report_index {
            Some(index) => {
                expected_diags.swap_remove(index);
            }
            None => {
                unexpected_diags.push(actual_diag);
            }
        }
    }

    if unexpected_diags.is_empty() && expected_diags.is_empty() {
        return true;
    }

    let all_diags = unexpected_diags
        .into_iter()
        .map(|unexpected_diag| {
            let error_diag =
                Diagnostic::new_error(format!("unexpected {}", unexpected_diag.severity.to_str()));

            if let Some(span) = unexpected_diag
                .labels
                .iter()
                .find(|label| label.style == LabelStyle::Primary)
                .map(|label| label.span)
            {
                error_diag
                    .with_label(Label::new_primary(span).with_message(unexpected_diag.message))
            } else {
                error_diag
            }
        })
        .chain(
            expected_diags
                .into_iter()
                .map(|expected_diag| expected_diag.to_error_diagnostic()),
        );

    emit_diagnostics_to_stderr(ccx.source_loader(), all_diags);
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
