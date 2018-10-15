use std::collections::HashMap;
use std::{cmp, fmt, fs, io, path};

use syntax::datum::Datum;
use syntax::span::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum SourceKind {
    /// Loaded from a file with the given filename
    File(String),
    /// Loaded from the REPL
    Repl,
    /// Loaded from an RFI module
    RfiModule(String, String),
}

impl fmt::Display for SourceKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            SourceKind::File(filename) => write!(formatter, "{}", filename),
            SourceKind::Repl => write!(formatter, "<repl input>"),
            SourceKind::RfiModule(filename, fun_name) => {
                write!(formatter, "{}:{}", filename, fun_name)
            }
        }
    }
}

pub struct SourceFile {
    span_offset: usize,
    kind: SourceKind,
    source: String,
}

new_indexing_id_type!(SourceFileId, usize);

impl SourceFile {
    pub fn span_offset(&self) -> usize {
        self.span_offset
    }

    pub fn span(&self) -> Span {
        Span {
            lo: self.span_offset as u32,
            hi: (self.span_offset + self.source.len()) as u32,
        }
    }

    pub fn kind(&self) -> &SourceKind {
        &self.kind
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn parse(&self) -> Result<Vec<Datum>, syntax::error::Error> {
        use syntax::parser::data_from_str_with_span_offset;
        data_from_str_with_span_offset(self.source(), self.span_offset())
    }
}

#[derive(Default)]
pub struct SourceLoader {
    source_files: Vec<SourceFile>,
    loaded_paths: HashMap<Box<path::Path>, SourceFileId>,
    pub(crate) next_span_offset: usize,
}

impl SourceLoader {
    pub fn new() -> SourceLoader {
        Self::default()
    }

    pub fn load_path(&mut self, path: &path::Path) -> Result<SourceFileId, io::Error> {
        if let Some(source_file_id) = self.loaded_paths.get(path) {
            return Ok(*source_file_id);
        }

        let kind = SourceKind::File(path.to_string_lossy().to_string());
        let source = fs::read_to_string(path)?;

        let source_file_id = self.load_string(kind, source);
        self.loaded_paths.insert(path.into(), source_file_id);

        Ok(source_file_id)
    }

    pub fn load_string(&mut self, kind: SourceKind, source: String) -> SourceFileId {
        let span_offset = self.next_span_offset;
        self.next_span_offset = span_offset + source.len();

        SourceFileId::new_entry_id(
            &mut self.source_files,
            SourceFile {
                span_offset,
                kind,
                source,
            },
        )
    }

    pub fn source_file(&self, id: SourceFileId) -> &SourceFile {
        &self.source_files[id.to_usize()]
    }

    pub fn source_files(&self) -> &Vec<SourceFile> {
        &self.source_files
    }
}

#[derive(PartialEq, Debug)]
pub struct SourceLoc<'src> {
    source_file_id: SourceFileId,
    line: usize,
    column: usize,

    snippet: &'src str,
    span_str: &'src str,
}

impl<'src> SourceLoc<'src> {
    pub fn calculate_from_span(source_loader: &'src SourceLoader, span: Span) -> SourceLoc<'src> {
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
            })
            .unwrap();

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
            source_file_id: SourceFileId::new(source_file_index),
            line,
            column,
            snippet: remaining_source,
            span_str,
        }
    }

    /// Returns the source file ID
    pub fn source_file_id(&self) -> SourceFileId {
        self.source_file_id
    }

    /// Returns the 0-indexed line
    pub fn line(&self) -> usize {
        self.line
    }

    /// Returns the 0-indexed column
    pub fn column(&self) -> usize {
        self.column
    }

    /// Returns a slice containing the characters of the span
    pub fn span_str(&self) -> &'src str {
        self.span_str
    }

    /// Returns a slice from the beginning of the span until the end of the file
    pub fn snippet(&self) -> &'src str {
        self.snippet
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn calculate_source_loc() {
        let mut source_loader = SourceLoader::new();

        let first_contents = "12\n34\n";
        let second_contents = "☃6";

        let first_kind = SourceKind::File("<first>".to_owned());
        let second_kind = SourceKind::File("<second>".to_owned());

        let first_file_id = source_loader.load_string(first_kind, first_contents.into());
        let second_file_id = source_loader.load_string(second_kind, second_contents.into());

        let test_cases = vec![
            (
                Span { lo: 0, hi: 1 },
                SourceLoc {
                    source_file_id: first_file_id,
                    line: 0,
                    column: 0,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[0..1],
                },
            ),
            (
                Span { lo: 1, hi: 2 },
                SourceLoc {
                    source_file_id: first_file_id,
                    line: 0,
                    column: 1,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[1..2],
                },
            ),
            (
                Span { lo: 2, hi: 4 },
                SourceLoc {
                    source_file_id: first_file_id,
                    line: 0,
                    column: 2,
                    snippet: &first_contents[0..],
                    span_str: &first_contents[2..4],
                },
            ),
            (
                Span { lo: 3, hi: 4 },
                SourceLoc {
                    source_file_id: first_file_id,
                    line: 1,
                    column: 0,
                    snippet: &first_contents[3..],
                    span_str: &first_contents[3..4],
                },
            ),
            (
                Span { lo: 6, hi: 9 },
                SourceLoc {
                    source_file_id: second_file_id,
                    line: 0,
                    column: 0,
                    snippet: &second_contents[0..],
                    span_str: &second_contents[0..3],
                },
            ),
            (
                Span { lo: 9, hi: 10 },
                SourceLoc {
                    source_file_id: second_file_id,
                    line: 0,
                    column: 1,
                    snippet: &second_contents[0..],
                    span_str: &second_contents[3..4],
                },
            ),
        ];

        for (span, expected) in test_cases {
            assert_eq!(
                expected,
                SourceLoc::calculate_from_span(&source_loader, span)
            );
        }
    }
}
