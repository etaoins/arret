#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

mod codegen;
pub mod error;
mod hir;
mod mir;
pub mod repl;
pub mod reporting;
mod rfi;
mod source;
mod ty;
mod typeck;

pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, Options as GenProgramOptions, OutputType};
pub use crate::hir::lowering::lower_program;
pub use crate::hir::PackagePaths;
pub use crate::id_type::ArcId;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::source::{SourceFile, SourceKind, SourceLoader, SourceLoc};
pub use crate::typeck::infer::infer_program;

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

    fn rfi_loader(&self) -> &rfi::Loader {
        &self.rfi_loader
    }
}
