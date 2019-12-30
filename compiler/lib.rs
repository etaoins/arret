#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod id_type;

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

use codespan_reporting::Diagnostic;
use std::collections::HashSet;
use std::sync::Arc;

pub use crate::codegen::initialise_llvm;
pub use crate::codegen::program::{gen_program, Options as GenProgramOptions, OutputType};
pub use crate::context::CompileCtx;
pub use crate::hir::PackagePaths;
pub use crate::id_type::ArcId;
pub use crate::mir::eval_hir::{BuiltProgram, EvalHirCtx};
pub use crate::mir::print_program as print_program_mir;
pub use crate::reporting::emit_diagnostics_to_stderr;
pub use crate::source::{SourceFile, SourceLoader};

pub struct EvaluableProgram {
    pub ehx: EvalHirCtx,
    pub main_var_id: hir::VarId,
    pub rfi_libraries: Vec<Arc<rfi::Library>>,
}

/// Visits a subtree of modules, evaluates their definitions and collects their RFI libraries
fn include_imports(
    ehx: &mut EvalHirCtx,
    visited_modules: &mut HashSet<hir::ModuleId>,
    rfi_libraries: &mut Vec<Arc<rfi::Library>>,
    root_module: &ArcId<context::Module>,
) -> Result<(), Vec<Diagnostic>> {
    if visited_modules.contains(&root_module.module_id) {
        return Ok(());
    }

    visited_modules.insert(root_module.module_id);

    if let Some(ref rfi_library) = root_module.rfi_library {
        rfi_libraries.push(rfi_library.clone());
    }

    // Make sure our imports are first
    for import in &root_module.imports {
        include_imports(ehx, visited_modules, rfi_libraries, import)?;
    }

    for def in &root_module.defs {
        ehx.visit_def(def)?;
    }

    Ok(())
}

pub fn program_to_evaluable(
    ccx: &CompileCtx,
    source_file: &SourceFile,
) -> Result<EvaluableProgram, Vec<Diagnostic>> {
    use crate::typeck::infer;

    let entry_module = ccx.source_file_to_module(source_file)?;

    let main_var_id = if let Some(var_id) = entry_module.main_var_id {
        var_id
    } else {
        use codespan_reporting::Label;
        let file_span = source_file.file_map().span();

        return Err(vec![Diagnostic::new_error(
            "no main! function defined in entry module",
        )
        .with_label(
            Label::new_primary(file_span).with_message("main! function expected in this file"),
        )]);
    };

    let inferred_main_type = &entry_module.inferred_locals[&main_var_id.local_id()];
    infer::ensure_main_type(&entry_module.defs, main_var_id, inferred_main_type)
        .map_err(|err| vec![err.into()])?;

    let mut ehx = EvalHirCtx::new(ccx.enable_optimisations());
    let mut rfi_libraries = vec![];

    for import in &entry_module.imports {
        include_imports(&mut ehx, &mut HashSet::new(), &mut rfi_libraries, import)?;
    }

    for def in entry_module.defs {
        // We can consume here because we own the entry module
        ehx.consume_def(def)?;
    }

    if ehx.should_collect() {
        ehx.collect_garbage();
    }

    Ok(EvaluableProgram {
        ehx,
        main_var_id,
        rfi_libraries,
    })
}
