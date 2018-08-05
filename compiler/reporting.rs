use std::cmp;
#[cfg(test)]
use std::fmt;
use std::iter;

use ansi_term::Colour;
use ansi_term::Style;

use source::{SourceKind, SourceLoader};
use syntax;
use syntax::span::Span;

#[derive(PartialEq, Debug)]
struct SourceLoc<'kind, 'src> {
    kind: &'kind SourceKind,
    line: usize,
    column: usize,

    snippet: &'src str,
    span_str: &'src str,
}

fn span_to_source_loc(source_loader: &SourceLoader, span: Span) -> SourceLoc {
    let source_files = source_loader.source_files();
    let lo = span.lo as usize;

    // Find the file we landed on
    let source_file_index = source_files
        .binary_search_by(|candidate_file| {
            if lo < candidate_file.span_offset() {
                cmp::Ordering::Greater
            } else if lo > (candidate_file.span_offset() + candidate_file.source().len()) {
                cmp::Ordering::Less
            } else {
                cmp::Ordering::Equal
            }
        }).unwrap();

    let source_file = &source_files[source_file_index];

    let mut remaining_bytes = lo - source_file.span_offset();
    let mut remaining_source = source_file.source();

    let mut line = 0;
    loop {
        assert!(remaining_bytes <= remaining_source.len());

        let next_line_pos = remaining_source
            .find('\n')
            .map(|i| i + 1)
            .unwrap_or_else(|| remaining_source.len());

        if remaining_bytes < next_line_pos {
            break;
        }

        remaining_bytes -= next_line_pos;
        remaining_source = &remaining_source[next_line_pos..];
        line += 1
    }

    let column = remaining_source[0..remaining_bytes].chars().count();

    let span_str = &remaining_source[remaining_bytes..];
    let span_str = &span_str[0..(span.hi - span.lo) as usize];

    SourceLoc {
        kind: source_file.kind(),
        line,
        column,
        snippet: remaining_source,
        span_str,
    }
}

fn print_span_snippet(source_loader: &SourceLoader, level: Level, span: Span) {
    let loc = span_to_source_loc(source_loader, span);
    print_loc_snippet(level, &loc)
}

fn print_loc_snippet(level: Level, loc: &SourceLoc) {
    let border_style = Colour::Blue.bold();

    eprintln!(
        "  {} {}:{}:{}",
        border_style.paint("-->"),
        loc.kind,
        loc.line + 1,
        loc.column + 1
    );

    let line_number_text = format!("{}", loc.line + 1);
    let line_number_chars = line_number_text.len();

    let snippet_next_newline = loc.snippet.find('\n').unwrap_or_else(|| loc.snippet.len());
    let snippet_first_line = &loc.snippet[0..snippet_next_newline];

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
    print_marker(level, loc, 1);
}

fn print_marker(level: Level, loc: &SourceLoc, column_offset: usize) {
    let chars = cmp::max(1, loc.span_str.chars().take_while(|c| c != &'\n').count());
    let marker_style = level.colour().bold();

    eprintln!(
        "{: <1$}{2}",
        "",
        loc.column + column_offset,
        marker_style.paint(iter::repeat("^").take(chars).collect::<String>())
    );
}

pub trait Reportable {
    fn level(&self) -> Level;
    fn message(&self) -> String;
    fn span(&self) -> Span;
    fn macro_invocation_span(&self) -> Option<Span> {
        None
    }

    fn report(&self, source_loader: &SourceLoader) {
        let default_bold = Style::new().bold();
        let level = self.level();
        let span = self.span();

        let post_error_snippet_loc = if span.is_empty() {
            None
        } else {
            let loc = span_to_source_loc(source_loader, span);
            if let (SourceKind::Repl(offset), 0) = (loc.kind, loc.line) {
                // Print a marker pointing at the REPL line the user entered
                print_marker(level, &loc, *offset);
                None
            } else {
                // Show the snippet after the error message
                Some(loc)
            }
        };

        eprintln!(
            "{}: {}",
            level.colour().bold().paint(level.name()),
            default_bold.paint(self.message())
        );

        if let Some(loc) = post_error_snippet_loc {
            print_loc_snippet(self.level(), &loc);
        }

        if let Some(macro_invocation_span) = self.macro_invocation_span() {
            eprintln!("{}", default_bold.paint("in this macro invocation"));
            print_span_snippet(source_loader, Level::Note, macro_invocation_span);
        }

        if let Some(associated_report) = self.associated_report() {
            // Skip the newline so the reports are visually grouped
            associated_report.report(source_loader);
        } else {
            eprintln!("");
        }
    }

    fn associated_report(&self) -> Option<Box<dyn Reportable>> {
        None
    }
}

#[cfg(test)]
impl fmt::Debug for Reportable {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(formatter, "Reportable({})", self.message())
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bytepos_to_source_loc_tests() {
        let mut source_loader = SourceLoader::new();

        let first_contents = "12\n34\n";
        let second_contents = "☃6";

        let first_kind = SourceKind::File("<first>".to_owned());
        let second_kind = SourceKind::File("<second>".to_owned());

        source_loader.load_string(first_kind.clone(), first_contents.into());
        source_loader.load_string(second_kind.clone(), second_contents.into());

        let test_cases = vec![
            (
                Span { lo: 0, hi: 1 },
                SourceLoc {
                    kind: &first_kind,
                    line: 0,
                    column: 0,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[0..1],
                },
            ),
            (
                Span { lo: 1, hi: 2 },
                SourceLoc {
                    kind: &first_kind,
                    line: 0,
                    column: 1,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[1..2],
                },
            ),
            (
                Span { lo: 2, hi: 4 },
                SourceLoc {
                    kind: &first_kind,
                    line: 0,
                    column: 2,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[2..4],
                },
            ),
            (
                Span { lo: 3, hi: 4 },
                SourceLoc {
                    kind: &first_kind,
                    line: 1,
                    column: 0,
                    snippet: &first_contents[3..],
                    span_str: &first_contents[3..4],
                },
            ),
            (
                Span { lo: 6, hi: 9 },
                SourceLoc {
                    kind: &second_kind,
                    line: 0,
                    column: 0,
                    snippet: &second_contents[0..],
                    span_str: &second_contents[0..3],
                },
            ),
            (
                Span { lo: 9, hi: 10 },
                SourceLoc {
                    kind: &second_kind,
                    line: 0,
                    column: 1,
                    snippet: &second_contents[0..],
                    span_str: &second_contents[3..4],
                },
            ),
        ];

        for (span, expected) in test_cases {
            assert_eq!(expected, span_to_source_loc(&source_loader, span));
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

    fn span(&self) -> Span {
        self.span()
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

    fn span(&self) -> Span {
        self.open_char_span
    }

    fn message(&self) -> String {
        format!("{} starts here", self.expected_content.description())
    }
}
