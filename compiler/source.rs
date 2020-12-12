use std::ffi::OsString;
use std::ops::Range;
use std::sync::{Arc, RwLock, RwLockReadGuard};
use std::{fmt, fs, io, path};

use codespan_reporting::files::Error as CodespanError;

use arret_syntax::datum::Datum;
use arret_syntax::span::{FileId, Span};

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
    source: SourceText,
    parsed: Result<Vec<Datum>, arret_syntax::error::Error>,
}

impl SourceFile {
    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn source(&self) -> &'_ str {
        self.source.as_ref()
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

struct ReportableFile {
    filename: OsString,
    source: SourceText,
    line_offsets: Vec<usize>,
}

impl ReportableFile {
    fn name(&self) -> String {
        self.filename.to_string_lossy().into()
    }

    fn source(&self) -> &str {
        self.source.as_ref()
    }

    fn line_index(&self, offset: usize) -> usize {
        match self
            .line_offsets
            .binary_search_by(|line_start| line_start.cmp(&offset))
        {
            Ok(line) => line,
            Err(line) => line - 1,
        }
    }

    fn line_range(&self, line_index: usize) -> Option<Range<usize>> {
        let start = self.line_offsets.get(line_index)?;

        let end = self
            .line_offsets
            .get(line_index + 1)
            .cloned()
            .unwrap_or_else(|| self.source.as_ref().len());

        Some(*start..end)
    }
}

#[derive(Default)]
pub struct SourceLoader {
    files: RwLock<Vec<ReportableFile>>,
}

impl SourceLoader {
    pub fn new() -> Self {
        Self::default()
    }

    /// Synchronously read path into a `SourceFile`
    pub fn load_path(&self, path: &path::Path) -> Result<SourceFile, io::Error> {
        let source = fs::read_to_string(path)?;

        Ok(self.load_string(
            path.as_os_str().to_owned(),
            SourceText::Shared(source.into()),
        ))
    }

    /// Loads a caller-provided string into a `SourceFile`
    pub fn load_string(&self, filename: OsString, source: impl Into<SourceText>) -> SourceFile {
        use arret_syntax::parser::data_from_str;

        let source = source.into();
        let reportable_file = ReportableFile {
            filename,
            line_offsets: codespan_reporting::files::line_starts(source.as_ref()).collect(),
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
            source,
        }
    }

    /// Reserves space for `additional` more files
    ///
    /// This can be used to avoid allocating memory under our instance's write lock.
    pub fn reserve(&self, additional: usize) {
        self.files.write().unwrap().reserve(additional)
    }

    /// Returns a `ReportableFiles` instance usable with `codespan-reporting`
    ///
    /// This will take our instance's read lock.
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

type CodespanResult<T> = Result<T, CodespanError>;

impl<'a> codespan_reporting::files::Files<'a> for ReportableFiles<'a> {
    type FileId = FileId;
    type Source = &'a str;
    type Name = String;

    fn name(&self, file_id: FileId) -> CodespanResult<String> {
        self.get_file(file_id)
            .ok_or(CodespanError::FileMissing)
            .map(|f| f.name())
    }

    fn source(&self, file_id: FileId) -> CodespanResult<&str> {
        self.get_file(file_id)
            .ok_or(CodespanError::FileMissing)
            .map(|f| f.source())
    }

    fn line_index(&self, file_id: FileId, offset: usize) -> CodespanResult<usize> {
        self.get_file(file_id)
            .ok_or(CodespanError::FileMissing)
            .map(|f| f.line_index(offset))
    }

    fn line_range(&self, file_id: FileId, line_index: usize) -> CodespanResult<Range<usize>> {
        self.get_file(file_id)
            .ok_or(CodespanError::FileMissing)
            .and_then(|f| {
                f.line_range(line_index).ok_or(CodespanError::LineTooLarge {
                    given: line_index,
                    max: f.line_offsets.len(),
                })
            })
    }
}
