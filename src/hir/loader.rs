use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;

use syntax::value::Value;
use syntax::span::Span;
use syntax::parser::data_from_str;
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

pub fn load_library_data(span: Span, library_name: &LibraryName) -> Result<Vec<Value>, Error> {
    let mut path_buf = PathBuf::new();

    path_buf.push("stdlib");
    for path_component in library_name.path.iter() {
        path_buf.push(path_component);
    }

    path_buf.push(format!("{}.rsp", library_name.terminal_name));

    let mut source_file = File::open(path_buf).map_err(|_| Error::LibraryNotFound(span))?;
    let mut source_string = String::new();

    source_file
        .read_to_string(&mut source_string)
        .map_err(|_| Error::LibraryNotFound(span))?;

    data_from_str(&source_string).map_err(|pe| Error::SyntaxError(pe))
}
