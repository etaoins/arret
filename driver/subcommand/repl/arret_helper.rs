use std::borrow::Cow;

use ansi_term::{Colour, Style};

use rustyline::validate::{ValidationContext, ValidationResult};

use arret_syntax::datum::DataStr;

use super::command::{HELP_COMMAND, QUIT_COMMAND, TYPE_ONLY_PREFIX};
use super::syntax::{error_context_for_eol, error_for_line, MAXIMUM_PARSED_LINE_LEN};

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

fn sorted_strings_prefixed_by<'a, T: AsRef<str>>(
    haystack: &'a [T],
    prefix: &'a str,
) -> impl Iterator<Item = &'a T> + 'a {
    // Use a binary search to find the start of the strings
    let start_pos = match haystack.binary_search_by(|needle| needle.as_ref().cmp(prefix)) {
        Ok(found) => found,
        Err(insert_idx) => insert_idx,
    };

    haystack[start_pos..]
        .iter()
        // Once we stop matching prefixes we're done
        .take_while(move |needle| needle.as_ref().starts_with(prefix))
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

        let is_command = prefix.starts_with('/');
        let is_first_identifier = pos == prefix.len();

        if is_command && !is_first_identifier {
            // Don't complete commands in illegal positions
            return Ok((0, vec![]));
        }

        let options = sorted_strings_prefixed_by(&self.all_names, prefix)
            .filter_map(|name| {
                if name.ends_with(suffix) {
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
    type Hint = String;

    fn hint(&self, line: &str, pos: usize, _: &rustyline::Context<'_>) -> Option<String> {
        use arret_syntax::error::WithinContext;
        use arret_syntax::parser::is_identifier_char;

        let within_context = error_context_for_eol(line);

        // If we're inside a string we shouldn't try to hint identifiers
        if let Some(WithinContext::String(_)) = within_context {
            return Some("\"".to_owned());
        }

        let last_ident_start = line
            .rfind(|c| !is_identifier_char(c))
            .map(|i| i + 1)
            .unwrap_or(0);

        let last_ident = &line[last_ident_start..];

        let is_command = last_ident.starts_with('/');
        let is_first_identifier = pos == last_ident.len();

        // Make sure we have at least one character and we don't complete commands mid-line
        if !(last_ident.is_empty() || (is_command && !is_first_identifier)) {
            for name in sorted_strings_prefixed_by(&self.all_names, last_ident) {
                // Don't suggest ourselves
                if name.len() != last_ident.len() {
                    return Some(name[last_ident.len()..].to_owned());
                }
            }
        }

        within_context
            .and_then(|within| within.expected_next())
            .map(|en| en.close_char().to_string())
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

        let error_start = error_span.start() as usize;
        let error_end = error_span.end() as usize;

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

impl rustyline::validate::Validator for ArretHelper {
    fn validate(
        &self,
        ctx: &mut ValidationContext<'_>,
    ) -> Result<ValidationResult, rustyline::error::ReadlineError> {
        match error_context_for_eol(ctx.input()) {
            Some(_) => Ok(ValidationResult::Incomplete),
            None => Ok(ValidationResult::Valid(None)),
        }
    }
}

impl rustyline::Helper for ArretHelper {}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_sorted_strings_prefixed_by(
        expected: &[&'static str],
        haystack: &[&'static str],
        needle: &'static str,
    ) {
        let expected_vec = expected.to_owned();
        let actual_vec: Vec<&str> = sorted_strings_prefixed_by(haystack, needle)
            .cloned()
            .collect();

        assert_eq!(expected_vec, actual_vec)
    }

    #[test]
    fn sorted_strings_prefixed_by_empty() {
        let haystack: &[&str] = &[];
        assert_sorted_strings_prefixed_by(&[], haystack, "foo");
    }

    #[test]
    fn sorted_strings_prefixed_by_missing_at_beginning() {
        // "foo" would be before this one
        let haystack = &["zoop"];
        assert_sorted_strings_prefixed_by(&[], haystack, "foo");
    }

    #[test]
    fn sorted_strings_prefixed_by_missing_in_middle() {
        // "foo" would be in the middle of these two
        let haystack = &["bar", "zoop"];
        assert_sorted_strings_prefixed_by(&[], haystack, "foo");
    }

    #[test]
    fn sorted_strings_prefixed_by_missing_at_end() {
        // "foo" would be after of these two
        let haystack = &["bar", "baz"];
        assert_sorted_strings_prefixed_by(&[], haystack, "foo");
    }

    #[test]
    fn sorted_strings_prefixed_by_only_self() {
        let haystack = &["bar", "baz", "foo"];
        assert_sorted_strings_prefixed_by(&["foo"], haystack, "foo");
    }

    #[test]
    fn strings_prefixed_by_only_other() {
        let haystack = &["bar", "baz", "foobar", "foobaz"];

        assert_sorted_strings_prefixed_by(&["foobar", "foobaz"], haystack, "foo");
    }

    #[test]
    fn strings_prefixed_by_self_and_other() {
        let haystack = &["bar", "baz", "foo", "foobar", "foobaz", "zoop"];

        assert_sorted_strings_prefixed_by(&["foo", "foobar", "foobaz"], haystack, "foo");
    }
}
