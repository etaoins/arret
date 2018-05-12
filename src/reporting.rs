use std::cmp;
use std::iter;

use ansi_term::Colour;
use ansi_term::Style;
use ctx::CompileContext;
use syntax::span::Span;

#[derive(PartialEq, Debug)]
struct HumanPos<'a> {
    display_name: String,
    line: usize,
    column: usize,
    snippet: &'a str,
    snippet_byte_off: usize,
}

fn bytepos_to_human_pos(ccx: &CompileContext, bp: u32) -> HumanPos {
    let loaded_files = ccx.loaded_files();

    // Find the file we landed on
    let mut remaining_bytes = bp as usize;
    let mut loaded_files_iter = loaded_files.iter();

    let loaded_file = loop {
        let next_file = loaded_files_iter.next().unwrap();
        let next_file_size = next_file.source().len();

        if remaining_bytes < next_file_size {
            break next_file;
        }

        remaining_bytes -= next_file_size;
    };

    let mut remaining_source = &loaded_file.source()[..];

    let mut line = 1;
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

    let mut column = 1;
    let mut ci_iter = remaining_source.char_indices();
    loop {
        let (off, _) = ci_iter.next().expect("Unable to find character on line");

        if remaining_bytes == off {
            break;
        }
        assert!(remaining_bytes > off);

        column += 1
    }

    HumanPos {
        display_name: loaded_file.display_name().clone(),
        line,
        column,
        snippet: remaining_source,
        snippet_byte_off: remaining_bytes,
    }
}

fn print_snippet(ccx: &CompileContext, level: Level, span: Span) {
    let border_style = Colour::Blue.bold();
    let marker_style = level.colour().bold();

    let hp = bytepos_to_human_pos(ccx, span.lo);

    eprintln!(
        "  {} {}:{}:{}",
        border_style.paint("-->"),
        hp.display_name,
        hp.line,
        hp.column
    );

    let line_number_text = format!("{}", hp.line);
    let line_number_chars = line_number_text.len();

    let snippet_next_newline = hp.snippet.find('\n').unwrap_or_else(|| hp.snippet.len());
    let snippet_first_line = &hp.snippet[0..snippet_next_newline];

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

    let marker_bytes = (span.hi - span.lo) as usize;
    let marker_lo = hp.snippet_byte_off;
    let marker_hi = marker_lo + marker_bytes;
    let marker_chars = cmp::max(1, hp.snippet[marker_lo..marker_hi].chars().count());

    eprintln!(
        " {: <1$}{2}",
        "",
        hp.column - 1,
        marker_style.paint(iter::repeat("^").take(marker_chars).collect::<String>())
    );
}

pub trait Reportable {
    fn level(&self) -> Level;
    fn message(&self) -> String;
    fn span(&self) -> Span;
    fn macro_invocation_span(&self) -> Option<Span> {
        None
    }

    fn report(&self, ccx: &CompileContext) {
        let default_bold = Style::new().bold();
        let level = self.level();

        eprintln!(
            "{}: {}",
            level.colour().bold().paint(level.name()),
            default_bold.paint(self.message())
        );

        let span = self.span();
        if !span.is_empty() {
            print_snippet(ccx, self.level(), span);
        }

        if let Some(macro_invocation_span) = self.macro_invocation_span() {
            eprintln!("{}", default_bold.paint("in this macro invocation"));
            print_snippet(ccx, Level::Note, macro_invocation_span);
        }

        if let Some(associated_report) = self.associated_report() {
            // Skip the newline so the reports are visually grouped
            associated_report.report(ccx);
        } else {
            eprintln!("");
        }
    }

    fn associated_report(&self) -> Option<Box<Reportable>> {
        None
    }
}

#[derive(Copy, Clone)]
pub enum Level {
    Error,
    Note,
    Help,
}

impl Level {
    fn name(&self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Note => "note",
            Level::Help => "help",
        }
    }

    fn colour(&self) -> Colour {
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
    use ctx::LoadedFile;

    #[test]
    fn bytepos_to_human_pos_tests() {
        let mut ccx = CompileContext::new();

        let first_contents = "12\n34\n";
        let second_contents = "☃6";

        ccx.add_loaded_file(LoadedFile::new(
            "<first>".to_owned(),
            first_contents.to_owned(),
        ));
        ccx.add_loaded_file(LoadedFile::new(
            "<second>".to_owned(),
            second_contents.to_owned(),
        ));

        let test_cases = vec![
            (
                0,
                HumanPos {
                    display_name: "<first>".to_owned(),
                    line: 1,
                    column: 1,
                    snippet: &first_contents[0..],
                    snippet_byte_off: 0,
                },
            ),
            (
                1,
                HumanPos {
                    display_name: "<first>".to_owned(),
                    line: 1,
                    column: 2,
                    snippet: &first_contents[0..],
                    snippet_byte_off: 1,
                },
            ),
            (
                2,
                HumanPos {
                    display_name: "<first>".to_owned(),
                    line: 1,
                    column: 3,
                    snippet: &first_contents[0..],
                    snippet_byte_off: 2,
                },
            ),
            (
                3,
                HumanPos {
                    display_name: "<first>".to_owned(),
                    line: 2,
                    column: 1,
                    snippet: &first_contents[3..],
                    snippet_byte_off: 0,
                },
            ),
            (
                6,
                HumanPos {
                    display_name: "<second>".to_owned(),
                    line: 1,
                    column: 1,
                    snippet: &second_contents[0..],
                    snippet_byte_off: 0,
                },
            ),
            (
                9,
                HumanPos {
                    display_name: "<second>".to_owned(),
                    line: 1,
                    column: 2,
                    snippet: &second_contents[0..],
                    snippet_byte_off: 3,
                },
            ),
        ];

        for (bp, expected) in test_cases {
            assert_eq!(expected, bytepos_to_human_pos(&ccx, bp));
        }
    }
}
