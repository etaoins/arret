use arret_syntax::span::ByteIndex;

use super::command::TYPE_ONLY_PREFIX;

/// Maximum line length we'll provide parser hints and error highlighting for
///
/// This requires parsing the whole line and we don't support incremental reparsing. This means
/// in the worst case of pasting a large line character-by-character we'll behave O(n!) with
/// with the size of the pasted line. This seems like a reasonable cutoff where a human isn't
/// typing the input.
pub const MAXIMUM_PARSED_LINE_LEN: usize = 512;

pub fn error_for_line(mut line: &str) -> Option<arret_syntax::error::Error> {
    use arret_syntax::parser::datum_from_str_with_span_offset;

    let span_offset = if line.starts_with(TYPE_ONLY_PREFIX) {
        line = &line[TYPE_ONLY_PREFIX.len()..];
        TYPE_ONLY_PREFIX.len()
    } else {
        0
    };

    // Is this a command?
    if line.starts_with('/') ||
    // Or empty?
    line.chars().all(char::is_whitespace) ||
    // Or is too large to parse interactively?
    line.len() > MAXIMUM_PARSED_LINE_LEN
    {
        return None;
    }

    datum_from_str_with_span_offset(line, ByteIndex(span_offset as u32)).err()
}

pub fn expected_content_for_line(line: &str) -> Option<arret_syntax::error::ExpectedContent> {
    error_for_line(line).and_then(|error| {
        if let arret_syntax::error::ErrorKind::Eof(expected_content) = error.kind() {
            Some(*expected_content)
        } else {
            None
        }
    })
}
