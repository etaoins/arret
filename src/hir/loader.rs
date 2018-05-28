use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

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
    span: Span,
    display_name: String,
    input_reader: &mut Read,
) -> Result<Vec<Datum>> {
    let span_offset = ccx.next_span_offset();

    let mut source = String::new();

    input_reader.read_to_string(&mut source).map_err(|_| {
        Error::new(
            span,
            ErrorKind::ReadError(display_name.clone().into_boxed_str()),
        )
    })?;

    let data = data_from_str_with_span_offset(&source, span_offset);

    // Add a space to allow us to position errors at EOF
    source.push(' ');

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

    path_buf.push("stdlib");
    for path_component in &module_name.path {
        path_buf.push(path_component.as_ref());
    }

    path_buf.push(format!("{}.rsp", module_name.terminal_name));

    let display_name = path_buf.to_string_lossy().to_string();
    let mut source_file =
        File::open(path_buf).map_err(|_| Error::new(span, ErrorKind::ModuleNotFound))?;

    load_module_data(ccx, span, display_name, &mut source_file)
}
