#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::io::Write;
use std::ops::Range;
use std::sync::Arc;
use std::{fs, io, path, process};

use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::files::Files as _;

use tempfile::NamedTempFile;

use arret_syntax::span::{FileId, Span};

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx, SourceText};

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
    Exact(FileId, Range<usize>),
    StartRange(FileId, Range<usize>),
}

impl ExpectedSpan {
    fn matches(&self, actual_file_id: FileId, actual_range: Range<usize>) -> bool {
        match self {
            ExpectedSpan::Exact(expected_file_id, expected_range) => {
                actual_file_id == *expected_file_id && actual_range == *expected_range
            }
            ExpectedSpan::StartRange(expected_file_id, expected_start_range) => {
                let actual_range_start: usize = actual_range.start;

                actual_file_id == *expected_file_id
                    && actual_range_start >= expected_start_range.start
                    && actual_range_start < expected_start_range.end
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
    fn matches(&self, actual: &Diagnostic<FileId>) -> bool {
        if self.expected_severity != actual.severity {
            return false;
        }

        if !actual.message.starts_with(&self.message_prefix[..]) {
            return false;
        }

        actual.labels.iter().any(|candidate_label| {
            self.span
                .matches(candidate_label.file_id, candidate_label.range.clone())
        })
    }

    /// Returns a diagnostic for reporting missing expectation
    fn to_error_diagnostic(&self) -> Diagnostic<FileId> {
        let (file_id, span_range) = match self.span {
            ExpectedSpan::Exact(ref file_id, ref span_range) => (file_id, span_range),
            ExpectedSpan::StartRange(ref file_id, ref span_range) => (file_id, span_range),
        };

        let span = Span::new(
            Some(*file_id),
            span_range.start as u32,
            span_range.end as u32,
        );

        Diagnostic::error()
            .with_message(format!(
                "expected {}",
                severity_name(self.expected_severity)
            ))
            .with_labels(vec![Label::primary(
                span.file_id().unwrap(),
                span.byte_range(),
            )
            .with_message(format!("{} ...", self.message_prefix))])
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
        if let Some(message_prefix) = marker_string.strip_prefix(prefix) {
            return (*severity, message_prefix);
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
    source_file: &arret_compiler::SourceFile,
) -> Vec<ExpectedDiagnostic> {
    let source = source_file.source();

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
                span: ExpectedSpan::StartRange(source_file.file_id(), *start_of_line_index..index),
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
            let (severity, message_prefix) = take_severity(marker_string);

            ExpectedDiagnostic {
                expected_severity: severity,
                message_prefix: message_prefix.into(),
                span: ExpectedSpan::Exact(source_file.file_id(), span_start..span_end),
            }
        }))
        .collect()
}

fn unexpected_diag_to_error_diagnostic(unexpected_diag: Diagnostic<FileId>) -> Diagnostic<FileId> {
    let unexpected_primary_label = unexpected_diag
        .labels
        .iter()
        .find(|label| label.style == codespan_reporting::diagnostic::LabelStyle::Primary)
        .cloned();

    Diagnostic::error()
        .with_message(format!(
            "unexpected {}",
            severity_name(unexpected_diag.severity)
        ))
        .with_labels(
            unexpected_primary_label
                .into_iter()
                .map(|unexpected_primary_label| {
                    Label::primary(
                        unexpected_primary_label.file_id,
                        unexpected_primary_label.range,
                    )
                    .with_message(unexpected_diag.message.clone())
                })
                .collect(),
        )
}

fn exit_with_run_output_difference(
    source_filename: String,
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
        stream_name, source_filename
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

    std::process::exit(1);
}

fn result_for_single_test(
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> Result<(), Vec<Diagnostic<FileId>>> {
    let (output_path, run_type) = {
        let arret_compiler::EvaluableProgram {
            mut ehx,
            main_export_id,
            linked_libraries,
        } = arret_compiler::program_to_evaluable(ccx, source_file)?;

        // Try evaluating if we're not supposed to panic
        if !matches!(test_type, TestType::Run(RunType::Error(_))) {
            ehx.eval_main_fun(main_export_id)?;
        }

        let run_type = if let TestType::Run(run_type) = test_type {
            run_type
        } else {
            return Ok(());
        };

        // And now compiling and running
        let mir_program = ehx.into_built_program(main_export_id)?;

        if mir_program.is_empty() {
            // Don't bother building
            return Ok(());
        }

        let gen_program_opts = arret_compiler::GenProgramOptions::new();
        let output_path = NamedTempFile::new().unwrap().into_temp_path();

        arret_compiler::gen_program(
            gen_program_opts,
            &linked_libraries,
            &mir_program,
            &output_path,
            None,
        );

        (output_path, run_type)
    };

    let mut process = process::Command::new(output_path.as_os_str());

    let expected_output = run_type.expected_output();
    let output = process
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .unwrap();

    match run_type {
        RunType::Pass(_) => {
            if !output.status.success() {
                // Dump any panic message from the test
                let _ = io::stderr().write_all(&output.stderr);

                return Err(vec![Diagnostic::error()
                    .with_message(format!(
                        "unexpected status {} returned from integration test",
                        output.status,
                    ))
                    .with_labels(vec![Label::primary(source_file.file_id(), 0..1)
                        .with_message("integration test file")])]);
            }
        }
        RunType::Error(_) => {
            // Code 1 is used by panic. This makes sure we didn't e.g. SIGSEGV.
            if output.status.code() != Some(1) {
                return Err(vec![Diagnostic::error()
                    .with_message(format!(
                        "unexpected status {} returned from integration test",
                        output.status,
                    ))
                    .with_labels(vec![Label::primary(source_file.file_id(), 0..1)
                        .with_message("integration test file")])]);
            }
        }
    }

    if expected_output.stderr != output.stderr {
        exit_with_run_output_difference(
            ccx.source_loader()
                .files()
                .name(source_file.file_id())
                .unwrap(),
            "stderr",
            &expected_output.stderr,
            &output.stderr,
        );
    }

    if expected_output.stdout != output.stdout {
        exit_with_run_output_difference(
            ccx.source_loader()
                .files()
                .name(source_file.file_id())
                .unwrap(),
            "stdout",
            &expected_output.stdout,
            &output.stdout,
        );
    }

    Ok(())
}

fn run_single_pass_test(
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
    test_type: TestType,
) -> bool {
    let result = result_for_single_test(ccx, source_file, test_type);

    if let Err(diagnostics) = result {
        emit_diagnostics_to_stderr(ccx.source_loader(), diagnostics);
        false
    } else {
        true
    }
}

fn run_single_compile_fail_test(
    ccx: &CompileCtx,
    source_file: &arret_compiler::SourceFile,
) -> bool {
    let result = result_for_single_test(ccx, source_file, TestType::CompileError);

    let mut expected_diags = extract_expected_diagnostics(source_file);
    let actual_diags = if let Err(diags) = result {
        diags
    } else {
        eprintln!(
            "Compilation unexpectedly succeeded for {}",
            ccx.source_loader()
                .files()
                .name(source_file.file_id())
                .unwrap()
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
        .map(unexpected_diag_to_error_diagnostic)
        .chain(
            expected_diags
                .into_iter()
                .map(|expected_diag| expected_diag.to_error_diagnostic()),
        );

    emit_diagnostics_to_stderr(ccx.source_loader(), all_diags);
    false
}

fn run_single_test(ccx: &CompileCtx, input_path: &path::Path, test_type: TestType) -> bool {
    let source = fs::read_to_string(input_path).unwrap();

    let source_file = ccx.source_loader().load_string(
        input_path.as_os_str().to_owned(),
        SourceText::Shared(source.into()),
    );

    if test_type == TestType::CompileError {
        run_single_compile_fail_test(ccx, &source_file)
    } else {
        run_single_pass_test(ccx, &source_file, test_type)
    }
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

    let expected_output = RunOutput { stdout, stderr };

    Some((entry.path(), TestType::Run(run_type(expected_output))))
}

#[test]
fn integration() {
    let package_paths = arret_compiler::PackagePaths::test_paths(None);
    let ccx = Arc::new(arret_compiler::CompileCtx::new(package_paths, true));

    use arret_compiler::initialise_llvm;
    initialise_llvm(false);

    let (send_test, recv_test) = crossbeam_channel::unbounded::<(path::PathBuf, TestType)>();
    let (send_failed_test, recv_failed_test) = crossbeam_channel::unbounded::<String>();

    let worker_threads: Vec<std::thread::JoinHandle<_>> = (0..num_cpus::get())
        .map(|i| {
            let ccx = Arc::clone(&ccx);
            let recv_test = recv_test.clone();
            let send_failed_test = send_failed_test.clone();

            std::thread::Builder::new()
                .name(format!("integration test worker thread {}", i))
                .spawn(move || {
                    for (input_path, test_type) in recv_test.iter() {
                        let test_successful =
                            run_single_test(&ccx, input_path.as_path(), test_type);

                        if !test_successful {
                            send_failed_test
                                .send(input_path.to_string_lossy().to_string())
                                .unwrap();
                        }
                    }
                })
                .unwrap()
        })
        .collect();

    // The main thread doesn't need these
    drop(send_failed_test);
    drop(recv_test);

    fs::read_dir("./tests/compile-error")
        .unwrap()
        .filter_map(|entry| entry_to_compile_test_tuple(entry, TestType::CompileError))
        .for_each(|t| send_test.send(t).unwrap());

    fs::read_dir("./tests/optimise")
        .unwrap()
        .filter_map(|entry| entry_to_compile_test_tuple(entry, TestType::Optimise))
        .for_each(|t| send_test.send(t).unwrap());

    fs::read_dir("./tests/run-pass")
        .unwrap()
        .filter_map(|entry| entry_to_run_test_tuple(entry, RunType::Pass))
        .for_each(|t| send_test.send(t).unwrap());

    fs::read_dir("./tests/run-error")
        .unwrap()
        .filter_map(|entry| entry_to_run_test_tuple(entry, RunType::Error))
        .for_each(|t| send_test.send(t).unwrap());

    drop(send_test);

    for thread in worker_threads {
        thread.join().unwrap();
    }

    let failed_tests: Vec<String> = recv_failed_test.iter().collect();
    if !failed_tests.is_empty() {
        let _ = writeln!(
            io::stderr(),
            "integration tests failed: {}",
            failed_tests.join(", ")
        );

        std::process::exit(1);
    }
}
