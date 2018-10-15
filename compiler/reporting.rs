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

fn print_span_snippet(source_loader: &SourceLoader, level: Level, span: Span) {
    let loc = SourceLoc::calculate_from_span(source_loader, span);
    print_source_snippet(source_loader, level, &loc)
}

fn print_source_snippet(source_loader: &SourceLoader, level: Level, loc: &SourceLoc<'_>) {
    let border_style = Colour::Blue.bold();

    // The filename/line/column isn't useful for REPL input
    let kind = source_loader.source_file(loc.source_file_id()).kind();
    if kind != &SourceKind::Repl {
        eprintln!(
            "  {} {}:{}:{}",
            border_style.paint("-->"),
            kind,
            loc.line() + 1,
            loc.column() + 1
        );
    }

    let line_number_text = format!("{}", loc.line() + 1);
    let line_number_chars = line_number_text.len();

    let snippet = loc.snippet();
    let snippet_next_newline = snippet.find('\n').unwrap_or_else(|| snippet.len());
    let snippet_first_line = &snippet[0..snippet_next_newline];

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
        snippet_first_line
    );

    print_border_line();

    let chars = cmp::max(1, loc.span_str().chars().take_while(|c| c != &'\n').count());
    let marker_style = level.colour().bold();
    eprintln!(
        "{: <1$}{2}",
        "",
        loc.column() + 1,
        marker_style.paint(iter::repeat("^").take(chars).collect::<String>())
    );
}

pub trait Reportable {
    fn level(&self) -> Level;
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
    let level = report.level();
    let loc_trace = report.loc_trace();

    eprintln!(
        "{}: {}",
        level.colour().bold().paint(level.name()),
        default_bold.paint(report.message())
    );

    let origin = loc_trace.origin();
    if let Some(origin) = origin.to_non_empty() {
        let source_loc = SourceLoc::calculate_from_span(source_loader, origin);
        print_source_snippet(source_loader, level, &source_loc);
    }

    if let Some(macro_invocation) = loc_trace.macro_invocation.to_non_empty() {
        // Frequently errors will point to the macro arguments.
        // Showing the whole invocation would just be noise.
        if !macro_invocation.contains(origin) {
            eprintln!("{}", default_bold.paint("in this macro invocation"));
            print_span_snippet(source_loader, Level::Note, macro_invocation);
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
pub enum Level {
    Error,
    Note,
    Help,
}

impl Level {
    pub fn name(self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Note => "note",
            Level::Help => "help",
        }
    }

    fn colour(self) -> Colour {
        match self {
            Level::Error => Colour::Red,
            Level::Note => Colour::Blue,
            Level::Help => Colour::Cyan,
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
            ErrorKind::UnexpectedChar(c) => format!("unexpected `{}`", c),
            ErrorKind::UnevenMap => "map literal must have an even number of values".to_owned(),
        }
    }

    fn loc_trace(&self) -> LocTrace {
        self.span().into()
    }

    fn level(&self) -> Level {
        Level::Error
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
    fn level(&self) -> Level {
        Level::Help
    }

    fn loc_trace(&self) -> LocTrace {
        self.open_char_span.into()
    }

    fn message(&self) -> String {
        format!("{} starts here", self.expected_content.description())
    }
}
