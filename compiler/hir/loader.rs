use std::collections::HashMap;
use std::path;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::rfi;
use crate::source::SourceFile;
use crate::CompileCtx;

pub struct PackagePath {
    arret_base: Box<path::Path>,
    native_rust_base: Box<path::Path>,
    target_rust_base: Box<path::Path>,
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

    /// Creates an instance including the `stdlib` package
    pub fn with_stdlib(arret_root_dir: &path::Path, target_triple: Option<&str>) -> PackagePaths {
        let mut pp = PackagePaths::empty();

        let native_rust_base = arret_root_dir.join("target");
        let target_rust_base = if let Some(target_triple) = target_triple {
            native_rust_base.join(target_triple)
        } else {
            native_rust_base.clone()
        };

        let stdlib_path = PackagePath {
            arret_base: arret_root_dir.join("stdlib/arret").into(),
            native_rust_base: native_rust_base.into(),
            target_rust_base: target_rust_base.into(),
        };

        pp.add_package("stdlib", stdlib_path);
        pp
    }

    /// Creates an instance for use in our internal unit and integration tests
    pub fn test_paths(target_triple: Option<&str>) -> PackagePaths {
        let parent_path = path::Path::new("..");
        Self::with_stdlib(parent_path, target_triple)
    }

    pub fn add_package(&mut self, package_name: &str, path: PackagePath) {
        self.paths.insert(package_name.into(), path);
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct ModuleName {
    package_name: DataStr,
    path: Vec<DataStr>,
    terminal_name: DataStr,
}

#[derive(Debug)]
pub enum LoadedModule {
    Source(SourceFile),
    Rust(rfi::Library),
}

impl ModuleName {
    pub fn new(package_name: DataStr, path: Vec<DataStr>, terminal_name: DataStr) -> ModuleName {
        ModuleName {
            package_name,
            path,
            terminal_name,
        }
    }

    pub fn is_rfi(&self) -> bool {
        self.path.is_empty() && self.terminal_name.as_ref() == "rust"
    }

    pub fn terminal_name(&self) -> &DataStr {
        &self.terminal_name
    }
}

pub fn load_module_by_name(
    ccx: &CompileCtx,
    span: Span,
    module_name: &ModuleName,
) -> Result<LoadedModule> {
    let package_path = if let Some(package_path) = ccx
        .package_paths()
        .paths
        .get(module_name.package_name.as_ref())
    {
        package_path
    } else {
        return Err(Error::new(span, ErrorKind::PackageNotFound));
    };

    if module_name.is_rfi() {
        ccx.rfi_loader()
            .load(
                span,
                ccx.source_loader(),
                &package_path.native_rust_base,
                &package_path.target_rust_base,
                &module_name.package_name,
            )
            .map(LoadedModule::Rust)
    } else {
        // Look for files starting in the package path
        let mut path_buf = path::PathBuf::new();
        path_buf.push(&package_path.arret_base);

        for path_component in &module_name.path {
            path_buf.push(path_component.as_ref());
        }

        path_buf.push(format!("{}.arret", module_name.terminal_name));
        let path = path_buf.as_path();

        let source_file = ccx
            .source_loader()
            .load_path(path)
            .map_err(|err| Error::from_module_io(span, path, &err))?;

        Ok(LoadedModule::Source(source_file))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::source::EMPTY_SPAN;

    fn load_stdlib_module(name: &'static str) -> Result<LoadedModule> {
        let ccx = CompileCtx::new(PackagePaths::test_paths(None), true);
        let module_name = ModuleName::new("stdlib".into(), vec![], name.into());

        load_module_by_name(&ccx, EMPTY_SPAN, &module_name)
    }

    #[test]
    fn load_stdlib_base() {
        let loaded_module = load_stdlib_module("base").unwrap();

        if let LoadedModule::Source(data) = loaded_module {
            assert!(!data.parsed().unwrap().is_empty());
        } else {
            panic!("Did not get source module; got {:?}", loaded_module);
        }
    }

    #[test]
    fn load_stdlib_rust() {
        // Ensure we can locate and load the module. The RFI itself is tested separately.
        let loaded_module = load_stdlib_module("rust").expect(
            "unable to load stdlib library; you may need to `cargo build` before running tests",
        );

        if let LoadedModule::Rust(rfi_module) = loaded_module {
            assert!(!rfi_module.exported_funs.is_empty());
        } else {
            panic!("Did not get Rust module; got {:?}", loaded_module);
        }
    }

    #[test]
    fn load_stdlib_missing() {
        let err = load_stdlib_module("notamodule").unwrap_err();

        if let ErrorKind::ModuleNotFound(_) = err.kind() {
        } else {
            panic!("Unexpected error kind: {:?}", err.kind())
        }
    }
}
