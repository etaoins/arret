#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::ffi::OsStr;
use std::io::Write;
use std::ops::Range;
use std::{env, fs, io, path, process};

use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use rayon::prelude::*;
use tempfile::NamedTempFile;

use arret_syntax::span::Span;

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx, SourceLoader};

#[derive(Clone, PartialEq)]
struct RunOutput {
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

#[derive(Clone, PartialEq)]
enum RunType {
    Pass(RunOutput),
    Error(RunOutput),
}

impl RunType {
    fn expected_output(&self) -> &RunOutput {
        match self {
            RunType::Pass(run_output) => run_output,
            RunType::Error(run_output) => run_output,
        }
    }
}

#[derive(Clone, PartialEq)]
enum TestType {
    CompileError,
    Optimise,
    Run(RunType),
}

#[derive(Debug)]
enum ExpectedSpan {
    Exact(Span),
    StartRange(codespan::FileId, Range<codespan::ByteIndex>),
}

impl ExpectedSpan {
    fn matches(&self, actual_span: Span) -> bool {
        match self {
            ExpectedSpan::Exact(span) => *span == actual_span,
            ExpectedSpan::StartRange(expected_file_id, span_range) => {
                let actual_span_start = actual_span.start();

                actual_span.file_id() == Some(*expected_file_id)
                    && actual_span_start >= span_range.start
                    && actual_span_start < span_range.end
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

        std::iter::once(&actual.primary_label)
            .chain(actual.secondary_labels.iter())
            .any(|candidate_label| {
                let candidate_span = Span::new(Some(candidate_label.file_id), candidate_label.span);
                self.span.matches(candidate_span)
            })
    }

    /// Returns a diagnostic for reporting missing expectation
    fn to_error_diagnostic(&self) -> Diagnostic {
        let span = match self.span {
            ExpectedSpan::Exact(span) => span,
            ExpectedSpan::StartRange(file_id, ref span_range) => Span::new(
                Some(file_id),
                codespan::Span::new(span_range.start, span_range.end),
            ),
        };

        Diagnostic::new_error(
            format!("expected {}", severity_name(self.expected_severity)),
            Label::new(
                span.file_id().unwrap(),
                span.codespan_span(),
                format!("{} ...", self.message_prefix),
            ),
        )
    }
}

fn take_severity(marker_string: &str) -> (Severity, &str) {
    for (prefix, severity) in &[
        (" BUG ", Severity::Bug),
        (" ERROR ", Severity::Error),
        (" WARNING ", Severity::Warning),
        (" HELP ", Severity::Help),
        (" NOTE ", Severity::Note),
    ] {
        if marker_string.starts_with(prefix) {
            return (*severity, &marker_string[prefix.len()..]);
        }
    }

    panic!("Unknown severity prefix for `{}`", marker_string)
}

fn severity_name(severity: Severity) -> &'static str {
    match severity {
        Severity::Bug => "bug",
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Help => "help",
        Severity::Note => "note",
    }
}

fn extract_expected_diagnostics(
    source_loader: &SourceLoader,
    source_file: &arret_compiler::SourceFile,
) -> Vec<ExpectedDiagnostic> {
    let files = source_loader.files();
    let source = files.source(source_file.file_id()).as_ref();

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
                    source_file.file_id(),
                    codespan::ByteIndex(*start_of_line_index as u32)
                        ..codespan::ByteIndex(index as u32),
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
                    Some(source_file.file_id()),
                    codespan::Span::new(
                        codespan::ByteIndex(span_start as u32),
                        codespan::ByteIndex(span_end as u32),
                    ),
                )),
            }
        }))
        .collect()
}

fn exit_with_run_output_difference(
    source_filename: &OsStr,
    stream_name: &str,
    expected: &[u8],
    actual: &[u8],
) {
    use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

    let mut expected_color = ColorSpec::new();
    expected_color.set_fg(Some(Color::Red));

    let mut actual_color = ColorSpec::new();
    actual_color.set_fg(Some(Color::Green));

    let stderr = StandardStream::stderr(ColorChoice::Auto);
    let mut stderr_lock = stderr.lock();

    writeln!(
        stderr_lock,
        "unexpected {} output from integration test {}\n",
        stream_name,
        source_filename.to_string_lossy()
    )
    .unwrap();

    write!(stderr_lock, "Expected: \"").unwrap();

    let _ = stderr_lock.set_color(&expected_color);
    stderr_lock.write_all(expected).unwrap();
    let _ = stderr_lock.reset();
    writeln!(stderr_lock, "\"").unwrap();

    write!(stderr_lock, "Actual:   \"").unwrap();

    let _ = stderr_lock.set_color(&actual_color);
    stderr_lock.write_all(actual).unwrap();
    let _ = stderr_lock.reset();
    writeln!(stderr_lock, "\"").unwrap();

    process::exit(1);
}

fn result_for_single_test(
    target_triple: Option<&str>,
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> Result<(), Vec<Diagnostic>> {
    let arret_compiler::EvaluableProgram {
        mut ehx,
        main_var_id,
        rfi_libraries,
    } = arret_compiler::program_to_evaluable(ccx, &source_file)?;

    // Try evaluating if we're not supposed to panic
    if let TestType::Run(RunType::Error(_)) = test_type {
    } else {
        ehx.eval_main_fun(main_var_id)?;
    }

    let run_type = if let TestType::Run(run_type) = test_type {
        run_type
    } else {
        return Ok(());
    };

    // And now compiling and running
    let mir_program = ehx.into_built_program(main_var_id)?;

    if mir_program.is_empty() {
        // Don't bother building
        return Ok(());
    }

    let output_path = NamedTempFile::new().unwrap().into_temp_path();

    let gen_program_opts = arret_compiler::GenProgramOptions::new()
        .with_target_triple(target_triple.as_ref().map(|x| &**x));

    arret_compiler::gen_program(
        gen_program_opts,
        &rfi_libraries,
        &mir_program,
        &output_path,
        None,
    );

    let mut process = process::Command::new(output_path.as_os_str());

    let expected_output = run_type.expected_output();
    let output = process
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .output()
        .unwrap();

    match run_type {
        RunType::Pass(_) => {
            if !output.status.success() {
                // Dump any panic message from the test
                let _ = io::stderr().write_all(&output.stderr);

                return Err(vec![Diagnostic::new_error(
                    format!(
                        "unexpected status {} returned from integration test",
                        output.status,
                    ),
                    Label::new(
                        source_file.file_id(),
                        codespan::Span::initial(),
                        "integration test file",
                    ),
                )]);
            }
        }
        RunType::Error(_) => {
            // Code 1 is used by panic. This makes sure we didn't e.g. SIGSEGV.
            if output.status.code() != Some(1) {
                return Err(vec![Diagnostic::new_error(
                    format!(
                        "unexpected status {} returned from integration test",
                        output.status,
                    ),
                    Label::new(
                        source_file.file_id(),
                        codespan::Span::initial(),
                        "integration test file",
                    ),
                )]);
            }
        }
    }

    if expected_output.stderr != output.stderr {
        exit_with_run_output_difference(
            ccx.source_loader().files().name(source_file.file_id()),
            "stderr",
            &expected_output.stderr,
            &output.stderr,
        );
    }

    if expected_output.stdout != output.stdout {
        exit_with_run_output_difference(
            ccx.source_loader().files().name(source_file.file_id()),
            "stdout",
            &expected_output.stdout,
            &output.stdout,
        );
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

    let mut expected_diags = extract_expected_diagnostics(ccx.source_loader(), source_file);
    let actual_diags = if let Err(diags) = result {
        diags
    } else {
        eprintln!(
            "Compilation unexpectedly succeeded for {}",
            ccx.source_loader()
                .files()
                .name(source_file.file_id())
                .to_string_lossy()
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
            Diagnostic::new_error(
                format!("unexpected {}", severity_name(unexpected_diag.severity)),
                Label::new(
                    unexpected_diag.primary_label.file_id,
                    unexpected_diag.primary_label.span,
                    unexpected_diag.message,
                ),
            )
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
    let source_file = ccx.source_loader().load_path(input_path).unwrap();

    if test_type == TestType::CompileError {
        run_single_compile_fail_test(target_triple, ccx, &source_file)
    } else {
        run_single_pass_test(target_triple, ccx, &source_file, test_type)
    }
}

fn parse_env_var<F>(name: &str) -> Option<F>
where
    F: std::str::FromStr,
{
    env::var_os(name)
        .and_then(|os_str| os_str.into_string().ok())
        .and_then(|s| s.parse::<F>().ok())
}

fn entry_is_arret_source(entry: &fs::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|file_name| !file_name.starts_with('.') && file_name.ends_with(".arret"))
        .unwrap_or(false)
}

fn read_or_empty_vec(filename: &path::Path) -> Result<Vec<u8>, io::Error> {
    match fs::read(filename) {
        Ok(data) => Ok(data),
        Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(vec![]),
        Err(err) => Err(err),
    }
}

fn entry_to_compile_test_tuple(
    entry: io::Result<fs::DirEntry>,
    test_type: TestType,
) -> Option<(path::PathBuf, TestType)> {
    let entry = entry.unwrap();

    if !entry_is_arret_source(&entry) {
        None
    } else {
        Some((entry.path(), test_type))
    }
}

fn entry_to_run_test_tuple<RT>(
    entry: io::Result<fs::DirEntry>,
    run_type: RT,
) -> Option<(path::PathBuf, TestType)>
where
    RT: FnOnce(RunOutput) -> RunType,
{
    let entry = entry.unwrap();

    if !entry_is_arret_source(&entry) {
        return None;
    }

    let stderr_filename = entry.path().with_extension("stderr");
    let stdout_filename = entry.path().with_extension("stdout");

    // These files may not exist - we'll treat them as empty
    let stderr = read_or_empty_vec(&stderr_filename).unwrap();
    let stdout = read_or_empty_vec(&stdout_filename).unwrap();

    let expected_output = RunOutput { stderr, stdout };

    Some((entry.path(), TestType::Run(run_type(expected_output))))
}

#[test]
fn integration() {
    let target_triple = parse_env_var::<String>("ARRET_TEST_TARGET_TRIPLE");

    let package_paths =
        arret_compiler::PackagePaths::test_paths(target_triple.as_ref().map(|t| &**t));
    let ccx = arret_compiler::CompileCtx::new(package_paths, true);

    use arret_compiler::initialise_llvm;
    initialise_llvm(target_triple.is_some());

    let compile_error_entries = fs::read_dir("./tests/compile-error")
        .unwrap()
        .filter_map(|entry| entry_to_compile_test_tuple(entry, TestType::CompileError));

    let optimise_entries = fs::read_dir("./tests/optimise")
        .unwrap()
        .filter_map(|entry| entry_to_compile_test_tuple(entry, TestType::Optimise));

    let run_pass_entries = fs::read_dir("./tests/run-pass")
        .unwrap()
        .filter_map(|entry| entry_to_run_test_tuple(entry, RunType::Pass));

    let run_error_entries = fs::read_dir("./tests/run-error")
        .unwrap()
        .filter_map(|entry| entry_to_run_test_tuple(entry, RunType::Error));

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
        let _ = writeln!(
            io::stderr(),
            "integration tests failed: {}",
            failed_tests.join(", ")
        );

        process::exit(1);
    }
}
