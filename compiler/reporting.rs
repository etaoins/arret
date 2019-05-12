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

    pub fn to_labels(&self) -> Vec<Label> {
        let primary_label = Label::new_primary(self.origin);

        match self.macro_invocation {
            Some(macro_invocation_span) if !macro_invocation_span.contains(self.origin) => {
                let secondary_label = Label::new_secondary(macro_invocation_span)
                    .with_message("in this macro invocation");

                vec![primary_label, secondary_label]
            }
            _ => vec![primary_label],
        }
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

/// Returns a diagnostic for the passed syntax errror
pub fn diagnostic_for_syntax_error(error: &arret_syntax::error::Error) -> Diagnostic {
    let message = error.kind().message();
    let diagnostic = Diagnostic::new_error(message);

    if let arret_syntax::error::ErrorKind::Eof(ref ec) = error.kind() {
        if let Some(open_char_span) = ec.open_char_span() {
            // Add a secondary label
            return diagnostic.with_label(
                Label::new_secondary(open_char_span)
                    .with_message(format!("{} starts here", ec.description())),
            );
        }
    }

    diagnostic
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
