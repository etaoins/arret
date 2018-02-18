use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;

use syntax::value::Value;
use syntax::span::Span;
use syntax::parser::data_from_str_with_span_offset;
use hir::error::Error;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct LibraryName {
    path: Vec<String>,
    terminal_name: String,
}

impl LibraryName {
    pub fn new(path: Vec<String>, terminal_name: String) -> LibraryName {
        LibraryName {
            path,
            terminal_name,
        }
    }
}

pub struct LoadedFile {
    display_name: String,
    source: String,
}

pub struct Loader {
    loaded_files: Vec<LoadedFile>,
    next_span_offset: usize,
}

impl Loader {
    pub fn new() -> Loader {
        Loader {
            loaded_files: vec![],
            next_span_offset: 0,
        }
    }

    pub fn load_module_data(
        &mut self,
        display_name: String,
        input_reader: &mut Read,
    ) -> Result<Vec<Value>, Error> {
        let span_offset = self.next_span_offset;

        let mut source = String::new();

        input_reader
            .read_to_string(&mut source)
            .map_err(|_| Error::ReadError(display_name.clone()))?;

        let data = data_from_str_with_span_offset(&source, span_offset)
            .map_err(|pe| Error::SyntaxError(pe))?;

        // Track this file for diagnostic reporting
        self.next_span_offset = span_offset + source.len();
        self.loaded_files.push(LoadedFile {
            source,
            display_name,
        });

        Ok(data)
    }

    pub fn load_library_data(
        &mut self,
        span: Span,
        library_name: &LibraryName,
    ) -> Result<Vec<Value>, Error> {
        let mut path_buf = PathBuf::new();

        path_buf.push("stdlib");
        for path_component in library_name.path.iter() {
            path_buf.push(path_component);
        }

        path_buf.push(format!("{}.rsp", library_name.terminal_name));

        let display_name = path_buf.to_string_lossy().into_owned();
        let mut source_file = File::open(path_buf).map_err(|_| Error::LibraryNotFound(span))?;

        self.load_module_data(display_name, &mut source_file)
    }
}
