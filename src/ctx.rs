pub struct LoadedFile {
    source: String,
    display_name: String,
}

impl LoadedFile {
    pub fn new(display_name: String, source: String) -> LoadedFile {
        LoadedFile {
            source,
            display_name,
        }
    }
}

pub struct CompileContext {
    loaded_files: Vec<LoadedFile>,
    next_span_offset: usize,
}

impl CompileContext {
    pub fn new() -> CompileContext {
        CompileContext {
            loaded_files: vec![],
            next_span_offset: 0,
        }
    }

    pub fn add_loaded_file(&mut self, loaded_file: LoadedFile) {
        self.next_span_offset = loaded_file.source.len();
        self.loaded_files.push(loaded_file);
    }

    pub fn next_span_offset(&self) -> usize {
        self.next_span_offset
    }
}
