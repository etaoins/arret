use std::collections::HashMap;
use std::{fs, io, path};

pub struct SourceFile {
    span_offset: usize,
    display_name: String,
    source: String,
}

new_indexing_id_type!(SourceFileId, usize);

impl SourceFile {
    pub fn span_offset(&self) -> usize {
        self.span_offset
    }

    pub fn display_name(&self) -> &str {
        &self.display_name
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}

#[derive(Default)]
pub struct SourceLoader {
    source_files: Vec<SourceFile>,
    loaded_paths: HashMap<Box<path::Path>, SourceFileId>,
    next_span_offset: usize,
}

impl SourceLoader {
    pub fn new() -> SourceLoader {
        Self::default()
    }

    pub fn load_path(&mut self, path: &path::Path) -> Result<SourceFileId, io::Error> {
        if let Some(source_file_id) = self.loaded_paths.get(path) {
            return Ok(*source_file_id);
        }

        let display_name = path.to_string_lossy().to_string();
        let source = fs::read_to_string(path)?;

        let source_file_id = self.load_string(display_name, source);
        self.loaded_paths.insert(path.into(), source_file_id);

        Ok(source_file_id)
    }

    pub fn load_string(&mut self, display_name: String, source: String) -> SourceFileId {
        let span_offset = self.next_span_offset;
        self.next_span_offset = span_offset + source.len();

        SourceFileId::new_entry_id(
            &mut self.source_files,
            SourceFile {
                span_offset,
                display_name,
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
