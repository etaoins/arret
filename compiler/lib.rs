#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

mod arret_root;
mod codegen;
mod context;
mod hir;
mod mir;
mod promise;
pub mod repl;
mod reporting;
mod rfi;
mod source;
mod ty;
mod typeck;

use std::collections::HashSet;
use std::sync::Arc;

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::span::FileId;

pub use crate::arret_root::{find_arret_root, FindArretRootError};
pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, Options as GenProgramOptions, OutputType};
pub use crate::context::{CompileCtx, LinkedLibrary};
pub use crate::hir::PackagePaths;
pub use crate::id_type::ArcId;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::mir::print_program as print_program_mir;
pub use crate::reporting::emit_diagnostics_to_stderr;
pub use crate::source::{SourceFile, SourceLoader, SourceText};

pub struct EvaluableProgram {
    pub ehx: EvalHirCtx,
    pub main_export_id: hir::ExportId,
    pub linked_libraries: Vec<Arc<LinkedLibrary>>,
}

/// Visits a subtree of modules, evaluates their definitions and collects their RFI libraries
fn include_imports(
    ehx: &mut EvalHirCtx,
    visited_modules: &mut HashSet<context::ModuleId>,
    linked_libraries: &mut Vec<Arc<LinkedLibrary>>,
    root_module: &context::Module,
) -> Result<(), Vec<Diagnostic<FileId>>> {
    if visited_modules.contains(&root_module.module_id) {
        return Ok(());
    }

    visited_modules.insert(root_module.module_id);

    if let Some(ref linked_library) = root_module.linked_library {
        linked_libraries.push(linked_library.clone());
    }

    // Make sure our imports are first
    for import in root_module.imports.values() {
        include_imports(ehx, visited_modules, linked_libraries, import)?;
    }

    ehx.visit_module_defs(root_module.module_id, &root_module.defs)?;

    Ok(())
}

pub fn program_to_evaluable(
    ccx: &CompileCtx,
    source_file: &SourceFile,
) -> Result<EvaluableProgram, Vec<Diagnostic<FileId>>> {
    use arret_syntax::span::Span;

    use crate::typeck::infer;

    let entry_module = ccx.source_file_to_module(source_file)?;

    let main_local_id = if let Some(local_id) = entry_module.main_local_id {
        local_id
    } else {
        use codespan_reporting::diagnostic::Label;

        return Err(vec![Diagnostic::error()
            .with_message("no main! function defined in entry module")
            .with_labels(vec![Label::primary(source_file.file_id(), 0..0)
                .with_message("main! function expected in this file")])]);
    };

    let inferred_main_type = &entry_module.inferred_locals[&main_local_id];

    infer::ensure_main_type(
        Span::new(Some(source_file.file_id()), 0, 0),
        &entry_module.defs,
        main_local_id,
        inferred_main_type,
    )
    .map_err(|err| vec![err.into()])?;

    let mut ehx = EvalHirCtx::new(ccx.enable_optimisations());
    let mut linked_libraries = vec![];
    let mut visited_modules = HashSet::new();

    for import in entry_module.imports.values() {
        include_imports(
            &mut ehx,
            &mut visited_modules,
            &mut linked_libraries,
            import,
        )?;
    }

    // We can consume here because we own the entry module
    ehx.consume_module_defs(entry_module.module_id, entry_module.defs)?;

    if ehx.should_collect() {
        ehx.collect_garbage();
    }

    Ok(EvaluableProgram {
        ehx,
        main_export_id: hir::ExportId::new(entry_module.module_id, main_local_id),
        linked_libraries,
    })
}
