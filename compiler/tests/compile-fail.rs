#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

use std::alloc::System;
use std::cell::RefCell;
use std::ops::Range;
use std::{fs, path};

use rayon::prelude::*;

use compiler::reporting::{report_to_stderr, Level, LocTrace, Reportable};
use compiler::SourceLoader;
use syntax::span::Span;

#[global_allocator]
static GLOBAL: System = System;

thread_local!(static SOURCE_LOADER: RefCell<SourceLoader> = RefCell::new(SourceLoader::new()));

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
    fn matches(&self, actual_report: &dyn Reportable) -> bool {
        let loc_trace = actual_report.loc_trace();
        let candidate_spans = &[loc_trace.origin(), loc_trace.macro_invocation()];

        candidate_spans.iter().any(|candidate_span| {
            self.span.matches(*candidate_span) && actual_report
                .message()
                .starts_with(&self.message_prefix[..])
        })
    }
}

impl Reportable for ExpectedReport {
    fn loc_trace(&self) -> LocTrace {
        (match self.span {
            ExpectedSpan::Exact(span) => span,
            ExpectedSpan::StartRange(ref span_range) => Span {
                lo: span_range.start as u32,
                hi: span_range.end as u32,
            },
        })
        .into()
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

fn extract_expected_reports(source_file: &compiler::SourceFile) -> Vec<ExpectedReport> {
    let source = source_file.source();
    let span_offset = source_file.span_offset();

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
                span: ExpectedSpan::StartRange(
                    span_offset + start_of_line_index..span_offset + index,
                ),
            }
        })
        .collect::<Vec<ExpectedReport>>();

    let mut spanned_reports = source.match_indices(";^").map(|(index, _)| {
        let span_length = source[index..].find(' ').expect("Cannot find level") - 1;

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
        let (level, marker_string) = take_level(marker_string);

        ExpectedReport {
            level,
            message_prefix: marker_string.into(),
            span: ExpectedSpan::Exact(Span {
                lo: (span_offset + span_start) as u32,
                hi: (span_offset + span_end) as u32,
            }),
        }
    });

    line_reports.extend(&mut spanned_reports);
    line_reports
}

fn collect_reports(
    source_loader: &mut SourceLoader,
    source_file_id: compiler::SourceFileId,
) -> Vec<Box<dyn Reportable>> {
    let mut err_objects = Vec::<Box<dyn Reportable>>::new();
    let package_paths = compiler::PackagePaths::test_paths();

    let hir = match compiler::lower_program(&package_paths, source_loader, source_file_id) {
        Ok(hir) => hir,
        Err(errs) => {
            for err in errs {
                err_objects.push(Box::new(err));
            }
            return err_objects;
        }
    };

    match compiler::infer_program(hir.defs, hir.main_var_id) {
        Ok(_) => {}
        Err(errs) => {
            for err in errs {
                err_objects.push(Box::new(err));
            }
            return err_objects;
        }
    }

    panic!(
        "Compilation unexpectedly succeeded for {}",
        source_loader.source_file(source_file_id).kind()
    )
}

fn run_single_test_with_source_loader(
    source_loader: &mut SourceLoader,
    input_path: &path::Path,
) -> bool {
    use std::io;

    let source_file_id = source_loader.load_path(input_path).unwrap();

    let mut expected_reports = extract_expected_reports(source_loader.source_file(source_file_id));
    let actual_reports = collect_reports(source_loader, source_file_id);

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
        eprintln!("Unexpected {}:", unexpected_report.level().name());
        report_to_stderr(&source_loader, unexpected_report.as_ref());
    }

    for expected_report in expected_reports {
        report_to_stderr(&source_loader, &expected_report);
    }

    false
}

fn run_single_test(input_path: &path::Path) -> bool {
    SOURCE_LOADER.with(|source_loader| {
        run_single_test_with_source_loader(&mut *source_loader.borrow_mut(), input_path)
    })
}

#[test]
fn compile_fail() {
    let entries = fs::read_dir("./tests/compile-fail").unwrap();

    let failed_tests = entries
        .par_bridge()
        .filter_map(|entry| {
            let input_path = entry.unwrap().path();

            if !run_single_test(input_path.as_path()) {
                Some(input_path.to_string_lossy().to_string())
            } else {
                None
            }
        })
        .collect::<Vec<String>>();

    if !failed_tests.is_empty() {
        panic!("compile-fail tests failed: {}", failed_tests.join(", "))
    }
}
