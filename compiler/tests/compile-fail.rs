extern crate compiler;
extern crate syntax;

use std::ops::Range;
use std::{fs, path};

use compiler::reporting::{Level, Reportable};
use syntax::span::Span;

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
                let actual_span_start = actual_span.lo as usize;
                actual_span_start >= span_range.start && actual_span_start < span_range.end
            }
        }
    }
}

#[derive(Debug)]
struct ExpectedReport {
    level: Level,
    message_prefix: String,
    span: ExpectedSpan,
}

impl ExpectedReport {
    fn matches(&self, actual_report: &Box<Reportable>) -> bool {
        self.span.matches(actual_report.span())
            && actual_report
                .message()
                .starts_with(&self.message_prefix[..])
    }
}

impl Reportable for ExpectedReport {
    fn span(&self) -> Span {
        match self.span {
            ExpectedSpan::Exact(span) => span,
            ExpectedSpan::StartRange(ref span_range) => Span {
                lo: span_range.start as u32,
                hi: span_range.end as u32,
            },
        }
    }

    fn level(&self) -> Level {
        Level::Help
    }

    fn message(&self) -> String {
        format!(
            "expected {} `{} ...`",
            self.level.name(),
            self.message_prefix
        )
    }
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

fn extract_expected_reports(source: &str) -> Vec<ExpectedReport> {
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
            let (level, marker_string) = take_level(marker_string);

            ExpectedReport {
                level,
                message_prefix: marker_string.into(),
                span: ExpectedSpan::StartRange(*start_of_line_index..index),
            }
        })
        .collect::<Vec<ExpectedReport>>();

    let mut spanned_reports = source
        .match_indices(";^")
        .map(|(index, _)| {
            let span_length = source[index..].find(' ').expect("Cannot find level") - 1;

            let start_of_line_index = &source[..index]
                .rfind('\n')
                .expect("Cannot have a spanned error on first line");

            let start_of_previous_line_index =
                &source[..*start_of_line_index].rfind('\n').unwrap_or(0);

            let end_of_line_index = &source[index..]
                .find('\n')
                .map(|i| i + index)
                .unwrap_or_else(|| source.len());

            let span_line_offset = index - start_of_line_index + 1;

            let span_start = start_of_previous_line_index + span_line_offset;
            let span_end = span_start + span_length;

            // Take from after the ;^^ to the end of the line
            let marker_string = &source[index + span_length + 1..*end_of_line_index];
            let (level, marker_string) = take_level(marker_string);

            ExpectedReport {
                level,
                message_prefix: marker_string.into(),
                span: ExpectedSpan::Exact(Span {
                    lo: span_start as u32,
                    hi: span_end as u32,
                }),
            }
        })
        .collect();

    line_reports.append(&mut spanned_reports);
    line_reports
}

fn collect_reports(
    ccx: &mut compiler::CompileContext,
    display_name: String,
    source: String,
) -> Vec<Box<Reportable>> {
    let mut err_objects = Vec::<Box<Reportable>>::new();

    let hir = match compiler::lower_program(ccx, display_name.clone(), source) {
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

fn run_single_test(display_name: String, input_path: path::PathBuf) -> bool {
    let mut ccx = compiler::CompileContext::new();
    let source = fs::read_to_string(input_path).unwrap();

    let mut expected_reports = extract_expected_reports(&source);
    let mut actual_reports = collect_reports(&mut ccx, display_name.clone(), source);

    let mut unexpected_reports = vec![];

    while let Some(actual_report) = actual_reports.pop() {
        let expected_report_index = expected_reports
            .iter()
            .position(|expected_report| expected_report.matches(&actual_report));

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

    for unexpected_report in unexpected_reports {
        eprintln!("Unexpected {}:", unexpected_report.level().name());
        unexpected_report.report(&ccx);
    }

    for expected_report in expected_reports {
        expected_report.report(&ccx);
    }

    false
}

#[test]
fn compile_fail() {
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
