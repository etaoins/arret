use std::iter;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use arret_syntax::span::{FileId, Span};

use crate::source::SourceLoader;

/// Traces the location of report through macro expansions
#[derive(Debug, PartialEq, Clone)]
pub struct LocTrace {
    origin: Span,
    macro_invocation: Option<Span>,
}

impl LocTrace {
    pub fn new(origin: Span, macro_invocation: Option<Span>) -> LocTrace {
        LocTrace {
            origin,
            macro_invocation,
        }
    }

    pub fn with_macro_invocation(self, macro_invocation: Span) -> LocTrace {
        LocTrace {
            macro_invocation: Some(macro_invocation),
            ..self
        }
    }

    pub fn origin(&self) -> Span {
        self.origin
    }

    pub fn macro_invocation(&self) -> Option<Span> {
        self.macro_invocation
    }

    pub fn label_macro_invocation(&self, diagnostic: Diagnostic<FileId>) -> Diagnostic<FileId> {
        match self.macro_invocation {
            Some(macro_invocation_span) if !macro_invocation_span.contains(self.origin) => {
                let secondary_label =
                    new_secondary_label(macro_invocation_span, "in this macro invocation");

                let new_labels = diagnostic
                    .labels
                    .iter()
                    .cloned()
                    .chain(iter::once(secondary_label))
                    .collect();

                diagnostic.with_labels(new_labels)
            }
            _ => diagnostic,
        }
    }
}

impl From<Span> for LocTrace {
    fn from(span: Span) -> LocTrace {
        LocTrace::new(span, None)
    }
}

/// Helper for converting a series of errors in to diagnostics
///
/// This is intended for use with `map_err`
pub fn errors_to_diagnostics<E: Into<Diagnostic<FileId>>>(
    errors: Vec<E>,
) -> Vec<Diagnostic<FileId>> {
    errors.into_iter().map(Into::into).collect()
}

/// Returns a diagnostic for the passed syntax errror
///
/// This is required because `arret-syntax` doesn't depend on `codespan-reporting`. It requires
/// its consumers to handle reporting themselves.
pub fn diagnostic_for_syntax_error(error: &arret_syntax::error::Error) -> Diagnostic<FileId> {
    let origin = error.span();
    let within = error.kind().within_context();

    let primary_label_message = within
        .and_then(|within| within.expected_next())
        .map(|en| en.description())
        .unwrap_or_else(|| "syntax error".to_owned());

    let primary_label = new_primary_label(origin, primary_label_message);

    let diagnostic = Diagnostic::error()
        .with_message(error.kind().message())
        .with_labels(vec![]);

    if let Some(within) = within {
        if let Some(open_char_span) = within.open_char_span() {
            let secondary_label = new_secondary_label(
                open_char_span,
                format!("{} starts here", within.description()),
            );

            return diagnostic.with_labels(vec![primary_label, secondary_label]);
        }
    }

    diagnostic.with_labels(vec![primary_label])
}

pub fn new_primary_label(span: Span, message: impl Into<String>) -> Label<FileId> {
    Label::primary(span.file_id().unwrap(), span.byte_range()).with_message(message)
}

pub fn new_secondary_label(span: Span, message: impl Into<String>) -> Label<FileId> {
    Label::secondary(span.file_id().unwrap(), span.byte_range()).with_message(message)
}

/// Emits a series of diagnostics to standard error
///
/// This ensures the diagnostics are emitted as a contiguous group even when multiple threads
/// are emitting concurrently.
pub fn emit_diagnostics_to_stderr(
    source_loader: &SourceLoader,
    diagnostics: impl IntoIterator<Item = Diagnostic<FileId>>,
) {
    use codespan_reporting::term;
    use termcolor::{ColorChoice, StandardStream};

    let config = term::Config::default();

    let stderr = StandardStream::stderr(ColorChoice::Auto);
    let mut stderr_lock = stderr.lock();

    for diagnostic in diagnostics {
        let _ = codespan_reporting::term::emit(
            &mut stderr_lock,
            &config,
            &source_loader.files(),
            &diagnostic,
        );
    }
}
