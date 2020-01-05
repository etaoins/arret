use std::borrow::Cow;

use ansi_term::{Colour, Style};

use arret_syntax::datum::DataStr;

use super::command::{HELP_COMMAND, QUIT_COMMAND, TYPE_ONLY_PREFIX};
use super::syntax::{error_for_line, expected_content_for_line, MAXIMUM_PARSED_LINE_LEN};

/// Completions that don't map to a bound value in scope
const UNBOUND_COMPLETIONS: &[&str] = &[
    TYPE_ONLY_PREFIX,
    QUIT_COMMAND,
    HELP_COMMAND,
    "true",
    "false",
    "##NaN",
    "##Inf",
    "##-Inf",
];

/// Implementation of Rustyline's `Helper` trait
pub struct ArretHelper {
    all_names: Vec<DataStr>,
}

impl ArretHelper {
    pub fn new(mut bound_names: Vec<DataStr>) -> ArretHelper {
        bound_names.extend(UNBOUND_COMPLETIONS.iter().map(|unbound| (*unbound).into()));
        bound_names.sort();

        ArretHelper {
            all_names: bound_names,
        }
    }
}

impl rustyline::completion::Completer for ArretHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        use arret_syntax::parser::is_identifier_char;

        let prefix_start = line[0..pos]
            .rfind(|c| !is_identifier_char(c))
            .map(|i| i + 1)
            .unwrap_or(0);

        let prefix = &line[prefix_start..pos];

        let suffix = if line.len() > pos {
            let suffix_end = line[pos..]
                .find(|c| !is_identifier_char(c))
                .map(|i| i + pos)
                .unwrap_or_else(|| line.len());
            &line[pos..suffix_end]
        } else {
            ""
        };

        let options = self
            .all_names
            .iter()
            .filter_map(|name| {
                if name.starts_with('/') && pos != prefix.len() {
                    // Commands can only appear at the beginning of the line
                    None
                } else if name.starts_with(prefix) && name.ends_with(suffix) {
                    Some((&name[0..name.len() - suffix.len()]).to_owned())
                } else {
                    None
                }
            })
            .collect();

        Ok((prefix_start, options))
    }
}

impl rustyline::hint::Hinter for ArretHelper {
    fn hint(&self, line: &str, pos: usize, _: &rustyline::Context<'_>) -> Option<String> {
        use arret_syntax::error::ExpectedContent;
        use arret_syntax::parser::is_identifier_char;

        let expected_content = expected_content_for_line(line);

        // If we're inside a string we shouldn't try to hint identifiers
        if let Some(ExpectedContent::String(_)) = expected_content {
            return Some("\"".to_owned());
        }

        let last_ident_start = line
            .rfind(|c| !is_identifier_char(c))
            .map(|i| i + 1)
            .unwrap_or(0);

        let last_ident = &line[last_ident_start..];

        if !last_ident.is_empty() {
            for name in self.all_names.iter() {
                if name.starts_with('/') && pos != last_ident.len() {
                    // Command not at the start of the line
                    continue;
                }

                if name.starts_with(last_ident) && name.len() > last_ident.len() {
                    return Some(name[last_ident.len()..].to_owned());
                }
            }
        }

        expected_content
            .and_then(|ec| ec.expected_close_char())
            .map(|c| c.to_string())
    }
}

impl rustyline::highlight::Highlighter for ArretHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        // See if we have an error
        let error_span = error_for_line(line).and_then(|error| {
            if let arret_syntax::error::ErrorKind::Eof(ec) = error.kind() {
                // We'll already be hinting at the end of the line so point to the opening char
                ec.open_char_span()
            } else {
                Some(error.span())
            }
        });

        let error_span = if let Some(error_span) = error_span {
            error_span
        } else {
            return line.into();
        };

        let error_start = error_span.start().to_usize();
        let error_end = error_span.end().to_usize();

        let prefix = &line[0..error_start];
        let error = &line[error_start..error_end];
        let suffix = &line[error_end..];

        let error_style = Colour::Red.bold();
        format!("{}{}{}", prefix, error_style.paint(error), suffix).into()
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        let prompt_style = Colour::Fixed(25); // DeepSkyBlue4 (#005faf)
        prompt_style.paint(prompt).to_string().into()
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        use arret_syntax::parser::is_identifier_char;

        if hint.chars().next().map(is_identifier_char) == Some(true) {
            // This is a name completion
            let name_style = Style::new().dimmed();
            name_style.paint(hint).to_string().into()
        } else {
            // This is an unexpected EOF hint
            let unexpected_eof_style = Colour::Red.bold();
            unexpected_eof_style.paint(hint).to_string().into()
        }
    }

    fn highlight_char(&self, line: &str, _pos: usize) -> bool {
        // Essentially any character can change highlighting:
        //
        // - Delimiters can change the structure of input
        // - Identifier characters can make char literals (e.g. \newline) change validity
        line.len() <= MAXIMUM_PARSED_LINE_LEN
    }
}

impl rustyline::validate::Validator for ArretHelper {}
impl rustyline::Helper for ArretHelper {}
