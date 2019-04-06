use crate::hir::PackagePaths;
use crate::rfi;
use crate::source::SourceLoader;

/// Shared context for compilation
///
/// This isn't specific to a given program or REPL session. It acts as a global cache of loaded
/// source files and Rust libraries; it should be reused whenever possible.
pub struct CompileCtx {
    package_paths: PackagePaths,
    enable_optimisations: bool,

    source_loader: SourceLoader,
    rfi_loader: rfi::Loader,
}

impl CompileCtx {
    pub fn new(package_paths: PackagePaths, enable_optimisations: bool) -> Self {
        Self {
            package_paths,
            enable_optimisations,

            source_loader: SourceLoader::new(),
            rfi_loader: rfi::Loader::new(),
        }
    }

    pub fn package_paths(&self) -> &PackagePaths {
        &self.package_paths
    }

    pub fn enable_optimisations(&self) -> bool {
        self.enable_optimisations
    }

    pub fn source_loader(&self) -> &SourceLoader {
        &self.source_loader
    }

    pub(crate) fn rfi_loader(&self) -> &rfi::Loader {
        &self.rfi_loader
    }
}
