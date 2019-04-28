use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};
use std::{cmp, fmt, fs, io, path};

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

use crate::id_type::ArcId;

#[derive(PartialEq, Debug, Clone)]
pub enum SourceKind {
    /// Loaded from a file with the given filename
    File(String),
    /// Loaded from the REPL
    Repl,
    /// Loaded from an RFI module
    RfiModule(RfiModuleKind),
}

#[derive(PartialEq, Debug, Clone)]
pub struct RfiModuleKind {
    pub filename: Arc<path::Path>,
    pub fun_name: &'static str,
}

impl fmt::Display for SourceKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            SourceKind::File(filename) => write!(formatter, "{}", filename),
            SourceKind::Repl => write!(formatter, "<repl input>"),
            SourceKind::RfiModule(RfiModuleKind { filename, fun_name }) => {
                write!(formatter, "{}:{}", filename.to_string_lossy(), fun_name)
            }
        }
    }
}

pub struct SourceFile {
    span: Span,
    kind: SourceKind,

    source: Cow<'static, str>,
    line_number_offsets: Box<[u32]>,
    parsed: Result<Vec<Datum>, arret_syntax::error::Error>,
}

impl SourceFile {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &SourceKind {
        &self.kind
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn source_line(&self, line: usize) -> &str {
        let start = self.line_number_offsets[line];
        let end = self
            .line_number_offsets
            .get(line + 1)
            .map(|offset| offset - 1) // Don't include the "\n"
            .unwrap_or_else(|| self.source.len() as u32);

        &self.source[start as usize..end as usize]
    }

    pub fn parsed(&self) -> Result<&[Datum], arret_syntax::error::Error> {
        match &self.parsed {
            Ok(data) => Ok(data),
            Err(err) => Err(err.clone()),
        }
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(formatter)
    }
}

#[derive(Default)]
pub struct SourceLoader {
    source_files: RwLock<Vec<ArcId<SourceFile>>>,
    loaded_paths: Mutex<HashMap<Box<path::Path>, ArcId<SourceFile>>>,
}

fn build_line_number_offsets(input: &str) -> Box<[u32]> {
    use std::iter;

    iter::once(0)
        .chain(input.match_indices('\n').map(|(i, _)| (i + 1) as u32))
        .collect()
}

impl SourceLoader {
    pub fn new() -> SourceLoader {
        Self::default()
    }

    pub fn load_path(&self, path: &path::Path) -> Result<ArcId<SourceFile>, io::Error> {
        let mut loaded_paths = self.loaded_paths.lock().unwrap();

        if let Some(source_file) = loaded_paths.get(path) {
            return Ok(source_file.clone());
        }

        let source_file = self.load_path_uncached(path)?;
        loaded_paths.insert(path.into(), source_file.clone());

        Ok(source_file)
    }

    pub fn load_path_uncached(&self, path: &path::Path) -> Result<ArcId<SourceFile>, io::Error> {
        let kind = SourceKind::File(path.to_string_lossy().to_string());
        let source = fs::read_to_string(path)?;

        Ok(self.load_string(kind, source.into()))
    }

    pub fn load_string(&self, kind: SourceKind, source: Cow<'static, str>) -> ArcId<SourceFile> {
        use arret_syntax::parser::data_from_str_with_span_offset;

        let mut source_files = self.source_files.write().unwrap();
        let span_offset = source_files.last().map(|x| x.span().end()).unwrap_or(0) + 1;

        let span = Span::new(span_offset, span_offset + (source.len() as u32));
        let parsed = data_from_str_with_span_offset(&source, span_offset);

        let source_file = ArcId::new(SourceFile {
            span,
            kind,

            line_number_offsets: build_line_number_offsets(&source),
            source,
            parsed,
        });

        source_files.push(source_file.clone());
        source_file
    }
}

#[derive(PartialEq, Debug)]
pub struct SourceLoc {
    source_file: ArcId<SourceFile>,
    file_byte_offset: u32,
    line: usize,
    column: usize,
}

impl<'src> SourceLoc {
    /// Calculates a source location from a byte index
    pub fn from_byte_index(source_loader: &SourceLoader, point: u32) -> SourceLoc {
        let source_files = source_loader.source_files.read().unwrap();

        // Find the file we landed on
        let source_file_index = source_files
            .binary_search_by(|candidate_file| {
                if point < candidate_file.span().start() {
                    cmp::Ordering::Greater
                } else if point > candidate_file.span().end() {
                    cmp::Ordering::Less
                } else {
                    cmp::Ordering::Equal
                }
            })
            .unwrap();

        let source_file = &source_files[source_file_index];

        // Now find the line
        let file_byte_offset = point - source_file.span().start();
        let line = match source_file
            .line_number_offsets
            .binary_search(&file_byte_offset)
        {
            Ok(line) => line,      // Exact hit
            Err(line) => line - 1, // Within in the line
        };

        let line_start = source_file.line_number_offsets[line];

        let column = source_file.source()[line_start as usize..file_byte_offset as usize]
            .chars()
            .count();

        SourceLoc {
            source_file: source_file.clone(),
            file_byte_offset,
            line,
            column,
        }
    }

    /// Returns the source file
    pub fn source_file(&self) -> &ArcId<SourceFile> {
        &self.source_file
    }

    /// Returns the offset in bytes from the beginning of the file
    pub fn file_byte_offset(&self) -> u32 {
        self.file_byte_offset
    }

    /// Returns the 0-indexed line
    pub fn line(&self) -> usize {
        self.line
    }

    /// Returns the 0-indexed character column
    pub fn column(&self) -> usize {
        self.column
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn calculate_source_loc() {
        let source_loader = SourceLoader::new();

        let first_contents = "12\n34\n";
        let second_contents = "☃6";

        let first_kind = SourceKind::File("<first>".to_owned());
        let second_kind = SourceKind::File("<second>".to_owned());

        let first_file = source_loader.load_string(first_kind, first_contents.into());
        let second_file = source_loader.load_string(second_kind, second_contents.into());

        let test_cases = vec![
            (
                1,
                SourceLoc {
                    source_file: first_file.clone(),
                    file_byte_offset: 0,
                    line: 0,
                    column: 0,
                },
            ),
            (
                2,
                SourceLoc {
                    source_file: first_file.clone(),
                    file_byte_offset: 1,
                    line: 0,
                    column: 1,
                },
            ),
            (
                3,
                SourceLoc {
                    source_file: first_file.clone(),
                    file_byte_offset: 2,
                    line: 0,
                    column: 2,
                },
            ),
            (
                4,
                SourceLoc {
                    source_file: first_file,
                    file_byte_offset: 3,
                    line: 1,
                    column: 0,
                },
            ),
            (
                8,
                SourceLoc {
                    source_file: second_file.clone(),
                    file_byte_offset: 0,
                    line: 0,
                    column: 0,
                },
            ),
            (
                11,
                SourceLoc {
                    source_file: second_file,
                    file_byte_offset: 3,
                    line: 0,
                    column: 1,
                },
            ),
        ];

        for (point, expected) in test_cases {
            assert_eq!(expected, SourceLoc::from_byte_index(&source_loader, point));
        }
    }
}
