extern crate compiler;

use compiler::reporting::{Level, Reportable};
use std::io::{Read, Seek, SeekFrom};
use std::ops::Range;
use std::{fs, path};

#[derive(Debug)]
struct ExpectedReport {
    level: Level,
    message_prefix: String,
    span_range: Range<usize>,
}

fn take_level(marker_string: &str) -> (Level, &str) {
    for (prefix, level) in &[
        (" ERROR ", Level::Error),
        (" NOTE ", Level::Note),
        (" HELP ", Level::Help),
    ] {
        if marker_string.starts_with(prefix) {
            return (*level, &marker_string[prefix.len()..]);
        }
    }

    panic!("Unknown level prefix for `{}`", marker_string)
}

fn extract_expected_reports(input_file: &mut fs::File) -> Vec<ExpectedReport> {
    let mut contents = String::new();
    input_file.read_to_string(&mut contents).unwrap();

    let expected_errors = contents
        .match_indices(";~")
        .map(|(index, _)| {
            let start_of_line_index = &contents[..index].rfind('\n').unwrap_or(0);

            let end_of_line_index = &contents[index..]
                .find('\n')
                .map(|i| i + index)
                .unwrap_or_else(|| contents.len());

            // Take from after the ;~ to the end of the line
            let marker_string = &contents[index + 2..*end_of_line_index];
            let (level, marker_string) = take_level(marker_string);

            ExpectedReport {
                level,
                message_prefix: marker_string.into(),
                span_range: *start_of_line_index..index,
            }
        })
        .collect();

    // Rewind the file so the compiler can read it
    input_file.seek(SeekFrom::Start(0)).unwrap();

    expected_errors
}

fn collect_reports(display_name: String, input_file: &mut fs::File) -> Vec<Box<Reportable>> {
    let mut ccx = compiler::CompileContext::new();
    let mut err_objects = Vec::<Box<Reportable>>::new();

    let hir = match compiler::lower_program(&mut ccx, display_name.clone(), input_file) {
        Ok(hir) => hir,
        Err(errs) => {
            for err in errs {
                err_objects.push(Box::new(err));
            }
            return err_objects;
        }
    };

    match compiler::infer_program(&hir.pvars, &hir.tvars, hir.defs) {
        Ok(_) => {}
        Err(errs) => {
            for err in errs {
                err_objects.push(Box::new(err));
            }
            return err_objects;
        }
    }

    panic!("Compilation unexpectedly succeeded for {}", display_name)
}

fn expected_matches_report(expected: &ExpectedReport, actual: &Box<Reportable>) -> bool {
    let actual_span_start = actual.span().lo as usize;

    actual.level() == expected.level
        && actual.message().starts_with(&expected.message_prefix[..])
        && actual_span_start >= expected.span_range.start
        && actual_span_start < expected.span_range.end
}

fn run_single_test(display_name: String, input_path: path::PathBuf) -> bool {
    let mut input_file = fs::File::open(input_path).unwrap();

    let mut expected_reports = extract_expected_reports(&mut input_file);
    if expected_reports.is_empty() {
        panic!("Unable to find an expected error in {}", display_name);
    }

    let mut actual_reports = collect_reports(display_name.clone(), &mut input_file);

    while let Some(expected_report) = expected_reports.pop() {
        let actual_report_index = actual_reports
            .iter()
            .position(|actual_report| expected_matches_report(&expected_report, actual_report));

        match actual_report_index {
            Some(index) => {
                actual_reports.swap_remove(index);
            }
            None => {
                eprintln!(
                    "Expected error in {}: `{}`",
                    display_name, expected_report.message_prefix
                );
                return false;
            }
        }
    }

    if let Some(unexpected_report) = actual_reports.pop() {
        eprintln!(
            "Unexpected error in {}: `{}`",
            display_name,
            unexpected_report.message()
        );

        return false;
    }

    true
}

#[test]
fn run_all_tests() {
    let entries = fs::read_dir("./tests/compile-fail").unwrap();
    let mut failed_tests = vec![];

    for entry in entries {
        let input_path = entry.unwrap().path();
        let display_name = input_path.to_string_lossy().to_string();

        if !run_single_test(display_name.clone(), input_path) {
            failed_tests.push(display_name);
        }
    }

    if !failed_tests.is_empty() {
        panic!("compile-fail tests failed: {}", failed_tests.join(", "))
    }
}
