use arret_syntax::span::Span;

use lsp_types;

#[derive(Debug)]
pub struct Document {
    version: Option<i64>,
    text: String,
    line_offsets: Vec<usize>,
}

fn line_offsets_for_str(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

impl Document {
    pub fn new(version: Option<i64>, text: String) -> Document {
        Document {
            version,
            line_offsets: line_offsets_for_str(&text),
            text,
        }
    }

    /// Returns a new instance of the document with specified range replaced
    pub fn with_range_edit(
        &self,
        new_version: Option<i64>,
        range: lsp_types::Range,
        new_range_text: &str,
    ) -> Result<Document, ()> {
        let start_offset = if let Some(start_offset) = self.position_to_offset(range.start) {
            start_offset
        } else {
            return Err(());
        };

        let end_offset = self.position_to_offset(range.end);

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

        Ok(Document {
            version: new_version,
            line_offsets: new_line_offsets,
            text: new_text,
        })
    }

    /// Returns the document version
    pub fn version(&self) -> Option<i64> {
        self.version
    }

    /// Returns the document text
    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    /// Returns an LSP `Range` for the given `arret-syntax` S`pan`
    pub fn span_to_range(&self, span: Span) -> lsp_types::Range {
        lsp_types::Range {
            start: self.offset_to_position(span.start() as usize),
            end: self.offset_to_position(span.end() as usize),
        }
    }

    /// Returns the position for the given byte offset
    pub fn offset_to_position(&self, offset: usize) -> lsp_types::Position {
        let line = match self
            .line_offsets
            .binary_search_by(|line_start| line_start.cmp(&offset))
        {
            Ok(line) => line,
            Err(line) => line - 1,
        };

        let line_start = self.line_offsets[line];
        let character: usize = self.text[line_start..offset]
            .chars()
            .map(|c| c.len_utf16())
            .sum();

        lsp_types::Position {
            line: line as u64,
            character: character as u64,
        }
    }

    /// Returns the byte offset for the given position
    fn position_to_offset(&self, position: lsp_types::Position) -> Option<usize> {
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
    fn test_positions() {
        let doc = Document::new(None, "Hello 💣\nNext line\n".into());

        assert_eq!(
            lsp_types::Position {
                line: 0,
                character: 0
            },
            doc.offset_to_position(0)
        );

        assert_eq!(
            lsp_types::Position {
                line: 0,
                character: 6
            },
            doc.offset_to_position(6)
        );

        assert_eq!(
            lsp_types::Position {
                line: 0,
                character: 8
            },
            doc.offset_to_position(10)
        );

        assert_eq!(
            lsp_types::Position {
                line: 1,
                character: 0
            },
            doc.offset_to_position(11)
        );

        assert_eq!(
            lsp_types::Position {
                line: 2,
                character: 0
            },
            doc.offset_to_position(21)
        );
    }

    #[test]
    fn test_append_to_empty() {
        let doc = Document::new(None, "".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 7,
                    },
                },
                "abc-123",
            )
            .unwrap();

        assert_eq!(&doc.text, "abc-123");
        assert_consistency(&doc);
    }

    #[test]
    fn test_append_to_line() {
        let doc = Document::new(None, "Hello".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 5,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 5,
                    },
                },
                ", world!",
            )
            .unwrap();

        assert_eq!(&doc.text, "Hello, world!");
        assert_consistency(&doc);
    }

    #[test]
    fn test_erase_all() {
        let doc = Document::new(None, "abc-123".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 7,
                    },
                },
                "",
            )
            .unwrap();

        assert_eq!(&doc.text, "");
        assert_consistency(&doc);
    }

    #[test]
    fn test_replace_line() {
        let doc = Document::new(None, "hello\nnebraska\n".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 1,
                        character: 8,
                    },
                },
                "world",
            )
            .unwrap();

        assert_eq!(&doc.text, "hello\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_insert_line() {
        let doc = Document::new(None, "hello\nworld\n".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 1,
                        character: 0,
                    },
                },
                "entire\n",
            )
            .unwrap();

        assert_eq!(&doc.text, "hello\nentire\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_delete_line() {
        let doc = Document::new(None, "hello\nentire\nworld\n".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 2,
                        character: 0,
                    },
                },
                "",
            )
            .unwrap();

        assert_eq!(&doc.text, "hello\nworld\n");
        assert_consistency(&doc);
    }

    #[test]
    fn test_delete_utf16() {
        let doc = Document::new(None, "Defuse 💣 me".into())
            .with_range_edit(
                None,
                lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 7,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 10,
                    },
                },
                "",
            )
            .unwrap();

        assert_eq!(&doc.text, "Defuse me");
        assert_consistency(&doc);
    }
}
