use std::path::PathBuf;
use std::{fs, io};

use ctx::{CompileContext, LoadedFile};
use hir::error::{Error, ErrorKind, Result};
use syntax::datum::Datum;
use syntax::parser::data_from_str_with_span_offset;
use syntax::span::Span;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
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

pub fn load_module_data(
    ccx: &mut CompileContext,
    display_name: String,
    source: String,
) -> Result<Vec<Datum>> {
    let span_offset = ccx.next_span_offset();
    let data = data_from_str_with_span_offset(&source, span_offset);

    // Track this file for diagnostic reporting
    ccx.add_loaded_file(LoadedFile::new(display_name, source));

    Ok(data?)
}

pub fn load_module_by_name(
    ccx: &mut CompileContext,
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

    path_buf.push(format!("{}.rsp", module_name.terminal_name));

    let display_name = path_buf.to_string_lossy().to_string();
    let source = fs::read_to_string(path_buf).map_err(|err| match err.kind() {
        io::ErrorKind::NotFound => Error::new(span, ErrorKind::ModuleNotFound),
        _ => Error::new(
            span,
            ErrorKind::ReadError(display_name.clone().into_boxed_str()),
        ),
    })?;

    load_module_data(ccx, display_name, source)
}
