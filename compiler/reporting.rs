use codespan_reporting::{Diagnostic, Label};

use arret_syntax::span::Span;

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

    pub fn label_macro_invocation(&self, diagnostic: Diagnostic) -> Diagnostic {
        match self.macro_invocation {
            Some(macro_invocation_span) if !macro_invocation_span.contains(self.origin) => {
                let macro_invocation_label = Label::new_secondary(macro_invocation_span)
                    .with_message("in this macro invocation");

                diagnostic.with_label(macro_invocation_label)
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
pub fn errors_to_diagnostics<E: Into<Diagnostic>>(errors: Vec<E>) -> Vec<Diagnostic> {
    errors.into_iter().map(Into::into).collect()
}

fn diagnostic_with_expected_content_labels(
    diagnostic: Diagnostic,
    origin: Span,
    ec: arret_syntax::error::ExpectedContent,
) -> Diagnostic {
    let primary_label_message = match ec.expected_close_char() {
        Some('"') => "expected `\"`".to_owned(),
        Some(sequence_close_char) => format!("expected datum or `{}`", sequence_close_char),
        None => "expected datum".to_owned(),
    };

    let diagnostic =
        diagnostic.with_label(Label::new_primary(origin).with_message(primary_label_message));

    if let Some(open_char_span) = ec.open_char_span() {
        diagnostic.with_label(
            Label::new_secondary(open_char_span)
                .with_message(format!("{} starts here", ec.description())),
        )
    } else {
        diagnostic
    }
}

/// Returns a diagnostic for the passed syntax errror
///
/// This is required because `arret-syntax` doesn't depend on `codespan-reporting`. It requires
/// its consumers to handle reporting themselves.
pub fn diagnostic_for_syntax_error(error: &arret_syntax::error::Error) -> Diagnostic {
    use arret_syntax::error::ErrorKind;

    let origin = error.span();
    let diagnostic = Diagnostic::new_error(error.kind().message());

    match error.kind() {
        ErrorKind::Eof(ec) | ErrorKind::UnexpectedChar(_, ec) => {
            diagnostic_with_expected_content_labels(diagnostic, origin, *ec)
        }
        _ => diagnostic.with_label(Label::new_primary(origin).with_message("syntax error")),
    }
}

/// Emits a series of diagnostics to standard error
///
/// This ensures the diagnostics are emitted as a contiguious group even when multiple threads
/// are emitting concurrently.
pub fn emit_diagnostics_to_stderr(
    source_loader: &SourceLoader,
    diagnostics: impl IntoIterator<Item = Diagnostic>,
) {
    use termcolor::{ColorChoice, StandardStream};
    let stderr = StandardStream::stderr(ColorChoice::Auto);
    let mut stderr_lock = stderr.lock();

    let code_map = source_loader.code_map();

    for diagnostic in diagnostics {
        let _ = codespan_reporting::emit(&mut stderr_lock, &code_map, &diagnostic);
    }
}
