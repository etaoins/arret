use lsp_types;

use std::sync::Arc;

pub struct Document {
    version: Option<i64>,
    text: Arc<str>,
    line_offsets: Vec<usize>,
}

fn line_offsets_for_str(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

impl Document {
    pub fn new(version: Option<i64>, text: Arc<str>) -> Document {
        Document {
            version,
            line_offsets: line_offsets_for_str(&text),
            text,
        }
    }

    pub fn replace_all(&mut self, new_version: Option<i64>, new_text: Arc<str>) {
        self.version = new_version;
        self.line_offsets = line_offsets_for_str(&new_text);
        self.text = new_text;
    }

    pub fn replace_range(
        &mut self,
        new_version: Option<i64>,
        range: lsp_types::Range,
        new_range_text: &str,
    ) -> Result<(), ()> {
        let start_offset = if let Some(start_offset) = self.position_for_offset(range.start) {
            start_offset
        } else {
            return Err(());
        };

        let end_offset = self.position_for_offset(range.end);

        // Rebuild the new text
        let mut new_text = self.text[..start_offset].to_string() + new_range_text;
        if let Some(end_offset) = end_offset {
            new_text += &self.text[end_offset..];
        }

        // Preserve the line offsets from before the edit
        let mut new_line_offsets = (&self.line_offsets[..=range.start.line as usize]).to_vec();

        // Add the line offsets inside the new range
        new_line_offsets.extend(
            new_range_text
                .match_indices('\n')
                .map(|(i, _)| i + start_offset + 1),
        );

        if let Some(end_offset) = end_offset {
            // Shift the remaining offsets to account for the size of the new range
            let previous_len = end_offset - start_offset;
            new_line_offsets.extend(
                self.line_offsets[range.end.line as usize + 1..]
                    .iter()
                    .map(|i| i + new_range_text.len() - previous_len),
            )
        }

        self.version = new_version;
        self.line_offsets = new_line_offsets;
        self.text = Arc::from(new_text);

        Ok(())
    }

    /// Returns the byte offset for the given position
    fn position_for_offset(&self, position: lsp_types::Position) -> Option<usize> {
        // Lines are already computed
        let line_offset = *self.line_offsets.get(position.line as usize)?;

        if position.character == 0 {
            return Some(line_offset);
        }

        let mut utf16_chars_remaining = position.character as usize;

        for (char_offset, c) in self.text[line_offset..].char_indices() {
            utf16_chars_remaining -= c.len_utf16();

            if utf16_chars_remaining == 0 {
                return Some(line_offset + char_offset + c.len_utf8());
            }
        }

        // Ran out of string
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_consistency(doc: &Document) {
        assert_eq!(line_offsets_for_str(&doc.text), doc.line_offsets);
    }

    #[test]
    fn test_append_to_empty() {
        let mut doc = Document::new(None, "".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 7
                    },
                },
                "abc-123"
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "abc-123");
        assert_consistency(&doc);
    }

    #[test]
    fn test_append_to_line() {
        let mut doc = Document::new(None, "Hello".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 5
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 5
                    },
                },
                ", world!"
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "Hello, world!");
        assert_consistency(&doc);
    }

    #[test]
    fn test_erase_all() {
        let mut doc = Document::new(None, "abc-123".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 7
                    },
                },
                ""
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "");
        assert_consistency(&doc);
    }

    #[test]
    fn test_replace_line() {
        let mut doc = Document::new(None, "hello\nnebraska\n".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0
                    },
                    end: lsp_types::Position {
                        line: 1,
                        character: 8
                    },
                },
                "world"
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "hello\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_insert_line() {
        let mut doc = Document::new(None, "hello\nworld\n".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0
                    },
                    end: lsp_types::Position {
                        line: 1,
                        character: 0
                    },
                },
                "entire\n"
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "hello\nentire\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_delete_line() {
        let mut doc = Document::new(None, "hello\nentire\nworld\n".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0
                    },
                    end: lsp_types::Position {
                        line: 2,
                        character: 0
                    },
                },
                ""
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "hello\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_delete_utf16() {
        let mut doc = Document::new(None, "Defuse 💣 me".into());

        assert!(doc
            .replace_range(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 7
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 10
                    },
                },
                ""
            )
            .is_ok());

        assert_eq!(doc.text.as_ref(), "Defuse me");
        assert_consistency(&doc);
    }
}
