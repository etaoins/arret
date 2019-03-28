use std::cmp;
#[cfg(test)]
use std::fmt;
use std::iter;

use ansi_term::Colour;
use ansi_term::Style;

use syntax;
use syntax::span::{Span, EMPTY_SPAN};

use crate::source::{SourceKind, SourceLoader, SourceLoc};

/// Traces the location of report through macro expansions
#[derive(Debug, PartialEq, Clone)]
pub struct LocTrace {
    origin: Span,
    macro_invocation: Span,
}

impl LocTrace {
    pub fn new(origin: Span, macro_invocation: Span) -> LocTrace {
        LocTrace {
            origin,
            macro_invocation,
        }
    }

    pub fn with_macro_invocation(self, macro_invocation: Span) -> LocTrace {
        LocTrace {
            macro_invocation,
            ..self
        }
    }

    pub fn origin(&self) -> Span {
        self.origin
    }

    pub fn macro_invocation(&self) -> Span {
        self.macro_invocation
    }
}

impl From<Span> for LocTrace {
    fn from(span: Span) -> LocTrace {
        LocTrace::new(span, EMPTY_SPAN)
    }
}

fn print_span_snippet(source_loader: &SourceLoader, severity: Severity, span: Span) {
    print_source_snippet(source_loader, severity, span)
}

fn print_source_snippet(source_loader: &SourceLoader, severity: Severity, span: Span) {
    let border_style = Colour::Blue.bold();

    let loc_start = SourceLoc::from_byte_index(source_loader, span.start());
    let loc_end = SourceLoc::from_byte_index(source_loader, span.end());

    // The filename/line/column isn't useful for REPL input
    let source_file = source_loader.source_file(loc_start.source_file_id());
    if source_file.kind() != &SourceKind::Repl {
        eprintln!(
            "  {} {}:{}:{}",
            border_style.paint("-->"),
            source_file.kind(),
            loc_start.line() + 1,
            loc_start.column() + 1
        );
    }

    let line_number_text = format!("{}", loc_start.line() + 1);
    let line_number_chars = line_number_text.len();

    let first_line = source_file.source_line(loc_start.line());
    let print_border_line = || {
        eprint!(
            "{: <1$} {2}",
            "",
            line_number_chars,
            border_style.paint("|")
        );
    };

    print_border_line();
    eprintln!("");

    eprintln!(
        "{} {} {}",
        border_style.paint(line_number_text),
        border_style.paint("|"),
        first_line
    );

    print_border_line();

    let chars = if loc_start.source_file_id() == loc_end.source_file_id()
        && loc_start.line() == loc_end.line()
    {
        cmp::max(loc_end.column() - loc_start.column(), 1)
    } else {
        1
    };

    let marker_style = severity.colour().bold();
    eprintln!(
        "{: <1$}{2}",
        "",
        loc_start.column() + 1,
        marker_style.paint(iter::repeat("^").take(chars).collect::<String>())
    );
}

pub trait Reportable {
    fn severity(&self) -> Severity;
    fn message(&self) -> String;
    fn loc_trace(&self) -> LocTrace;

    fn associated_report(&self) -> Option<Box<dyn Reportable>> {
        None
    }
}

#[cfg(test)]
impl fmt::Debug for dyn Reportable {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "Reportable({})", self.message())
    }
}

pub fn report_to_stderr(source_loader: &SourceLoader, report: &dyn Reportable) {
    let default_bold = Style::new().bold();
    let severity = report.severity();
    let loc_trace = report.loc_trace();

    eprintln!(
        "{}: {}",
        severity.colour().bold().paint(severity.to_str()),
        default_bold.paint(report.message())
    );

    let origin = loc_trace.origin();
    if let Some(origin) = origin.to_non_empty() {
        print_source_snippet(source_loader, severity, origin);
    }

    if let Some(macro_invocation) = loc_trace.macro_invocation.to_non_empty() {
        // Frequently errors will point to the macro arguments.
        // Showing the whole invocation would just be noise.
        if !macro_invocation.contains(origin) {
            eprintln!("{}", default_bold.paint("in this macro invocation"));
            print_span_snippet(source_loader, Severity::Note, macro_invocation);
        }
    }

    if let Some(associated_report) = report.associated_report() {
        // Skip the newline so the reports are visually grouped
        report_to_stderr(source_loader, associated_report.as_ref());
    } else {
        eprintln!("");
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Severity {
    Error,
    Note,
    Help,
}

impl Severity {
    pub fn to_str(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Note => "note",
            Severity::Help => "help",
        }
    }

    fn colour(self) -> Colour {
        match self {
            Severity::Error => Colour::Red,
            Severity::Note => Colour::Blue,
            Severity::Help => Colour::Cyan,
        }
    }
}

impl Reportable for syntax::error::Error {
    fn message(&self) -> String {
        use syntax::error::ErrorKind;

        match self.kind() {
            ErrorKind::Eof(ref ec) => {
                format!("unexpected end of file while parsing {}", ec.description())
            }
            ErrorKind::UnsupportedDispatch => "unsupported dispatch".to_owned(),
            ErrorKind::UnsupportedChar => "unsupported character".to_owned(),
            ErrorKind::InvalidCodePoint => "invalid code point".to_owned(),
            ErrorKind::UnsupportedStringEscape => "unsupported string escape".to_owned(),
            ErrorKind::IntegerOverflow => "integer literal does not fit in i64".to_owned(),
            ErrorKind::InvalidFloat => "unable to parse float".to_owned(),
            ErrorKind::UnexpectedChar(c) => format!("unexpected `{}`", c),
            ErrorKind::UnevenMap => "map literal must have an even number of values".to_owned(),
            ErrorKind::InvalidArgLiteral => {
                "arg literal must be `%`, `%{integer}` or `%&`".to_owned()
            }
        }
    }

    fn loc_trace(&self) -> LocTrace {
        self.span().into()
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn associated_report(&self) -> Option<Box<dyn Reportable>> {
        if let syntax::error::ErrorKind::Eof(ref ec) = self.kind() {
            if let Some(open_char_span) = ec.open_char_span() {
                return Some(Box::new(ContentStartHelp {
                    expected_content: *ec,
                    open_char_span,
                }));
            }
        }

        None
    }
}

struct ContentStartHelp {
    expected_content: syntax::error::ExpectedContent,
    open_char_span: Span,
}

impl Reportable for ContentStartHelp {
    fn severity(&self) -> Severity {
        Severity::Help
    }

    fn loc_trace(&self) -> LocTrace {
        self.open_char_span.into()
    }

    fn message(&self) -> String {
        format!("{} starts here", self.expected_content.description())
    }
}
