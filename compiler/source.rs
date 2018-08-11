use std::collections::HashMap;
use std::{fmt, fs, io, path};

#[derive(PartialEq, Debug, Clone)]
pub enum SourceKind {
    /// Loaded from a file with the given filename
    File(String),
    /// Loaded from the REPL with the span starting at the given terminal column
    Repl(usize),
    /// Loaded from an RFI module
    RfiModule(String, String),
}

impl fmt::Display for SourceKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            SourceKind::File(filename) => write!(formatter, "{}", filename),
            SourceKind::Repl(_) => write!(formatter, "<repl input>"),
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

    pub fn kind(&self) -> &SourceKind {
        &self.kind
    }

    pub fn source(&self) -> &str {
        &self.source
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
