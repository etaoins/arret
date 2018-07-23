use std::io;
use std::path::PathBuf;

use hir::error::{Error, ErrorKind, Result};
use source::{SourceFile, SourceLoader};
use syntax::datum::Datum;
use syntax::parser::data_from_str_with_span_offset;
use syntax::span::Span;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct ModuleName {
    path: Vec<Box<str>>,
    terminal_name: Box<str>,
}

impl ModuleName {
    pub fn new(path: Vec<Box<str>>, terminal_name: Box<str>) -> ModuleName {
        ModuleName {
            path,
            terminal_name,
        }
    }
}

pub fn parse_module_data(source_file: &SourceFile) -> Result<Vec<Datum>> {
    Ok(data_from_str_with_span_offset(
        source_file.source(),
        source_file.span_offset(),
    )?)
}

pub fn load_module_by_name(
    source_loader: &mut SourceLoader,
    span: Span,
    module_name: &ModuleName,
) -> Result<Vec<Datum>> {
    let mut path_buf = PathBuf::new();

    // TODO: Properly handle paths to the stdlib
    path_buf.push("..");

    path_buf.push("stdlib");
    for path_component in &module_name.path {
        path_buf.push(path_component.as_ref());
    }

    path_buf.push(format!("{}.arret", module_name.terminal_name));

    let source_file_id = source_loader.load_path(path_buf.clone()).map_err(|err| {
        match err.kind() {
            io::ErrorKind::NotFound => Error::new(span, ErrorKind::ModuleNotFound),
            _ => Error::new(span, ErrorKind::ReadError(Box::new(path_buf))),
        }
    })?;

    parse_module_data(source_loader.source_file(source_file_id))
}
