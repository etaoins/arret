use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock, RwLockReadGuard};
use std::{fmt, fs, io, path};

use codespan::FileName;

use arret_syntax::datum::Datum;

use crate::id_type::ArcId;

pub type CodeMap = codespan::CodeMap<Cow<'static, str>>;
pub type FileMap = codespan::FileMap<Cow<'static, str>>;

pub struct SourceFile {
    file_map: Arc<FileMap>,
    parsed: Result<Vec<Datum>, arret_syntax::error::Error>,
}

impl SourceFile {
    pub fn file_map(&self) -> &Arc<FileMap> {
        &self.file_map
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
        self.file_map.name().fmt(formatter)
    }
}

#[derive(Default)]
pub struct SourceLoader {
    code_map: RwLock<CodeMap>,
    loaded_paths: Mutex<HashMap<Box<path::Path>, ArcId<SourceFile>>>,
}

impl SourceLoader {
    pub fn new() -> SourceLoader {
        Self::default()
    }

    pub fn code_map(&self) -> RwLockReadGuard<'_, CodeMap> {
        self.code_map.read().unwrap()
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
        let file_name = FileName::Real(path.to_owned());
        let source = fs::read_to_string(path)?;

        Ok(self.load_string(file_name, source.into()))
    }

    pub fn load_string(&self, file_name: FileName, source: Cow<'static, str>) -> ArcId<SourceFile> {
        use arret_syntax::parser::data_from_str_with_span_offset;

        let file_map = self
            .code_map
            .write()
            .unwrap()
            .add_filemap(file_name, source);

        let span_offset = file_map.span().start();
        let parsed = data_from_str_with_span_offset(file_map.src(), span_offset);

        ArcId::new(SourceFile { file_map, parsed })
    }
}
