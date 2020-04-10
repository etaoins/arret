use std::ffi::OsString;
use std::ops::Range;
use std::sync::{Arc, RwLock, RwLockReadGuard};
use std::{fmt, fs, io, path};

use codespan_reporting;

use arret_syntax::datum::Datum;
use arret_syntax::span::{FileId, Span};

#[cfg(test)]
pub const EMPTY_SPAN: Span = Span::new(None, 0, 0);

#[derive(Clone)]
pub enum SourceText {
    Static(&'static str),
    Shared(Arc<str>),
}

impl AsRef<str> for SourceText {
    fn as_ref(&self) -> &str {
        match self {
            SourceText::Shared(shared) => shared.as_ref(),
            SourceText::Static(static_str) => static_str,
        }
    }
}

impl From<Arc<str>> for SourceText {
    fn from(s: Arc<str>) -> Self {
        SourceText::Shared(s)
    }
}

impl From<String> for SourceText {
    fn from(s: String) -> Self {
        SourceText::Shared(s.into())
    }
}

impl From<&'static str> for SourceText {
    fn from(s: &'static str) -> Self {
        SourceText::Static(s)
    }
}

pub struct SourceFile {
    file_id: FileId,
    parsed: Result<Vec<Datum>, arret_syntax::error::Error>,
}

impl SourceFile {
    pub fn file_id(&self) -> FileId {
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

fn line_offsets_for_str(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

struct ReportableFile {
    filename: OsString,
    source: SourceText,
    line_offsets: Vec<usize>,
}

#[derive(Default)]
pub struct SourceLoader {
    files: RwLock<Vec<ReportableFile>>,
}

impl SourceLoader {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_path(&self, path: &path::Path) -> Result<SourceFile, io::Error> {
        let source = fs::read_to_string(path)?;

        Ok(self.load_string(
            path.as_os_str().to_owned(),
            SourceText::Shared(source.into()),
        ))
    }

    pub fn load_string(&self, filename: OsString, source: impl Into<SourceText>) -> SourceFile {
        use arret_syntax::parser::data_from_str;

        let source = source.into();
        let reportable_file = ReportableFile {
            filename,
            line_offsets: line_offsets_for_str(source.as_ref()),
            source: source.clone(),
        };

        let file_index = {
            let mut files_write = self.files.write().unwrap();

            files_write.push(reportable_file);
            files_write.len()
        };

        let file_id = FileId::new(file_index as u32).unwrap();
        SourceFile {
            file_id,
            parsed: data_from_str(Some(file_id), source.as_ref()),
        }
    }

    /// Creates an artificial span for a Rust source file
    ///
    /// This points to a zero length span with the specified filename. It's intended for IR and code
    /// generated from the compiler that can't be directly attributed to input Arret code. For
    /// example, the compiler will generate synthetic functions on demand that can be shared between
    /// multiple call sites.
    pub fn span_for_rust_source_file(&self, filename: &'static str) -> Span {
        let reportable_file = ReportableFile {
            filename: filename.into(),
            source: SourceText::Static(""),
            line_offsets: vec![],
        };

        let file_index = {
            let mut files_write = self.files.write().unwrap();

            files_write.push(reportable_file);
            files_write.len()
        };

        let file_id = FileId::new(file_index as u32).unwrap();
        Span::new(Some(file_id), 0, 0)
    }

    pub fn files(&self) -> ReportableFiles<'_> {
        ReportableFiles {
            files: self.files.read().unwrap(),
        }
    }
}

pub struct ReportableFiles<'a> {
    files: RwLockReadGuard<'a, Vec<ReportableFile>>,
}

impl<'a> ReportableFiles<'a> {
    fn get_file(&self, file_id: FileId) -> Option<&ReportableFile> {
        self.files.get((file_id.get() - 1) as usize)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for ReportableFiles<'a> {
    type FileId = FileId;
    type Source = &'a str;
    type Name = String;

    fn name(&self, file_id: FileId) -> Option<String> {
        self.get_file(file_id)
            .map(|f| f.filename.to_string_lossy().into())
    }

    fn source(&self, file_id: FileId) -> Option<&str> {
        self.get_file(file_id).map(|f| f.source.as_ref())
    }

    fn line_index(&self, file_id: FileId, offset: usize) -> Option<usize> {
        self.get_file(file_id).map(|f| {
            match f
                .line_offsets
                .binary_search_by(|line_start| line_start.cmp(&offset))
            {
                Ok(line) => line,
                Err(line) => line - 1,
            }
        })
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> Option<Range<usize>> {
        self.get_file(file_id).and_then(|f| {
            let start = f.line_offsets.get(line_index)?;
            let end = f
                .line_offsets
                .get(line_index + 1)
                .cloned()
                .unwrap_or_else(|| f.source.as_ref().len());

            Some(*start..end)
        })
    }
}
