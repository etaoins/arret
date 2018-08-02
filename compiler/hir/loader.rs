use std::collections::HashMap;
use std::path;

use hir::error::{Error, ErrorKind, Result};
use hir::rfi;
use source::{SourceFile, SourceLoader};
use syntax::datum::Datum;
use syntax::parser::data_from_str_with_span_offset;
use syntax::span::Span;

pub struct PackagePath {
    arret_base: Box<path::Path>,
    rust_base: Box<path::Path>,
}

pub struct PackagePaths {
    paths: HashMap<Box<str>, PackagePath>,
}

impl PackagePaths {
    pub fn empty() -> PackagePaths {
        PackagePaths {
            paths: HashMap::new(),
        }
    }

    pub fn default() -> PackagePaths {
        let mut default = PackagePaths::empty();

        let stdlib_path = PackagePath {
            arret_base: path::Path::new("../stdlib/arret").into(),
            rust_base: path::Path::new("../target").into(),
        };

        default.add_package("stdlib", stdlib_path);

        default
    }

    pub fn add_package(&mut self, package_name: &str, path: PackagePath) {
        self.paths.insert(package_name.into(), path);
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct ModuleName {
    package_name: Box<str>,
    path: Vec<Box<str>>,
    terminal_name: Box<str>,
}

pub enum LoadedModule {
    Source(Vec<Datum>),
    Rust(rfi::Module),
}

impl ModuleName {
    pub fn new(package_name: Box<str>, path: Vec<Box<str>>, terminal_name: Box<str>) -> ModuleName {
        ModuleName {
            package_name,
            path,
            terminal_name,
        }
    }

    pub fn is_rfi(&self) -> bool {
        self.path.is_empty() && self.terminal_name.as_ref() == "rust"
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
    rfi_loader: &mut rfi::Loader,
    span: Span,
    package_paths: &PackagePaths,
    module_name: &ModuleName,
) -> Result<LoadedModule> {
    let package_path =
        if let Some(package_path) = package_paths.paths.get(&module_name.package_name) {
            package_path
        } else {
            return Err(Error::new(span, ErrorKind::PackageNotFound));
        };

    if module_name.is_rfi() {
        rfi_loader
            .load(
                span,
                source_loader,
                &package_path.rust_base,
                &module_name.package_name,
            ).map(LoadedModule::Rust)
    } else {
        // Look file files starting in the package path
        let mut path_buf = path::PathBuf::new();
        path_buf.push(&package_path.arret_base);

        for path_component in &module_name.path {
            path_buf.push(path_component.as_ref());
        }

        path_buf.push(format!("{}.arret", module_name.terminal_name));
        let path = path_buf.as_path();

        let source_file_id = source_loader
            .load_path(path)
            .map_err(|err| Error::from_module_io(span, path, &err))?;

        parse_module_data(source_loader.source_file(source_file_id)).map(LoadedModule::Source)
    }
}
