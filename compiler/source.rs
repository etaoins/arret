use std::sync::{RwLock, RwLockReadGuard};
use std::{fmt, fs, io, path};

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

#[cfg(test)]
pub const EMPTY_SPAN: Span = Span::new(None, codespan::Span::initial());

pub struct SourceFile {
    file_id: codespan::FileId,
    parsed: Result<Vec<Datum>, arret_syntax::error::Error>,
}

impl SourceFile {
    pub fn file_id(&self) -> codespan::FileId {
        self.file_id
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
        self.file_id.fmt(formatter)
    }
}

#[derive(Default)]
pub struct SourceLoader {
    files: RwLock<codespan::Files>,
}

impl SourceLoader {
    pub fn new() -> SourceLoader {
        Self::default()
    }

    pub fn files(&self) -> RwLockReadGuard<'_, codespan::Files> {
        self.files.read().unwrap()
    }

    pub fn load_path(&self, path: &path::Path) -> Result<SourceFile, io::Error> {
        let source = fs::read_to_string(path)?;
        Ok(self.load_string(path.to_string_lossy().into_owned(), source))
    }

    pub fn load_string(&self, filename: String, source: String) -> SourceFile {
        use arret_syntax::parser::data_from_str;

        let file_id = self.files.write().unwrap().add(filename, source);

        // TODO: This is parsing while keeping `files` locked
        let parsed = data_from_str(Some(file_id), self.files.read().unwrap().source(file_id));

        SourceFile { file_id, parsed }
    }

    /// Creates an artifical span for a Rust source file
    ///
    /// This points to a zero length span with the specified filename. It's intended for IR and code
    /// generated from the compiler that can't be directly attributed to input Arret code. For
    /// example, the compiler will generate synthetic functions on demand that can be shared between
    /// multiple callsites.
    pub fn span_for_rust_source_file(&self, filename: &'static str) -> Span {
        let file_id = self.files.write().unwrap().add(filename, "");
        Span::new(Some(file_id), codespan::Span::initial())
    }
}
