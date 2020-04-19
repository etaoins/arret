use crate::hir::PackagePaths;
use crate::rfi;
use crate::source::SourceLoader;

use std::collections::{HashMap, HashSet};
use std::hash;
use std::sync::Arc;

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::datum::Datum;
use arret_syntax::span::{FileId, Span};

use crate::hir;
use crate::hir::exports::Exports;
use crate::hir::import;
use crate::hir::loader::{LoadedModule, ModuleName};
use crate::hir::lowering::LoweredModule;
use crate::promise::PromiseMap;
use crate::reporting::diagnostic_for_syntax_error;
use crate::reporting::errors_to_diagnostics;
use crate::source::SourceFile;
use crate::ty;
use crate::typeck::infer;

new_global_id_type!(
    ModuleId,
    u32,
    std::sync::atomic::AtomicU32,
    std::num::NonZeroU32
);

pub(crate) struct ModuleImports {
    pub imports: HashSet<Arc<Module>>,
    pub imported_exports: HashMap<ModuleName, Arc<Exports>>,
}

/// Module being compiled until type inference
///
/// This represents both Arret and RFI libraries
pub(crate) struct Module {
    pub module_id: ModuleId,

    pub imports: HashSet<Arc<Module>>,
    pub defs: Vec<hir::Def<hir::Inferred>>,
    pub inferred_locals: Arc<HashMap<hir::LocalId, ty::Ref<ty::Poly>>>,
    pub exports: Arc<Exports>,
    pub main_var_id: Option<hir::VarId>,

    pub rfi_library: Option<Arc<rfi::Library>>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.module_id == other.module_id
    }
}

impl Eq for Module {}

impl hash::Hash for Module {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.module_id.get());
    }
}

pub(crate) type CachedModule = Result<Arc<Module>, Vec<Diagnostic<FileId>>>;
pub(crate) type UncachedModule = Result<Module, Vec<Diagnostic<FileId>>>;

/// Finds all transitive dependencies for a set of imports
///
/// This is inclusive of the imports themselves.
fn transitive_deps(imports: &HashSet<Arc<Module>>) -> HashSet<Arc<Module>> {
    let mut all_deps = imports.clone();

    for import in imports.iter() {
        all_deps.extend(transitive_deps(&import.imports).into_iter());
    }

    all_deps
}

fn prims_to_module(exports: Exports) -> UncachedModule {
    Ok(Module {
        module_id: ModuleId::alloc(),

        imports: HashSet::new(),
        defs: vec![],
        inferred_locals: Arc::new(HashMap::new()),
        exports: Arc::new(exports),
        main_var_id: None,

        rfi_library: None,
    })
}

fn rfi_library_to_module(span: Span, rfi_library: rfi::Library) -> UncachedModule {
    use crate::hir::var_id::VarIdAlloc;
    use crate::ty::Ty;

    use arret_syntax::datum::DataStr;

    let exported_funs = rfi_library.exported_funs();

    let module_id = ModuleId::alloc();
    let mut via = VarIdAlloc::new(module_id);

    let mut exports = HashMap::with_capacity(exported_funs.len());
    let mut defs = Vec::with_capacity(exported_funs.len());
    let mut inferred_locals = HashMap::with_capacity(exported_funs.len());

    for (fun_name, rust_fun) in exported_funs.iter() {
        let var_id = via.alloc_mut();
        let arret_type: ty::Ref<ty::Poly> =
            Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into();

        let fun_name_data_str: DataStr = (*fun_name).into();

        let def = hir::Def::<hir::Inferred> {
            span,
            macro_invocation_span: None,
            destruc: hir::destruc::Destruc::Scalar(
                span,
                hir::destruc::Scalar::new(
                    Some(var_id),
                    fun_name_data_str.clone(),
                    arret_type.clone(),
                ),
            ),
            value_expr: hir::Expr {
                result_ty: arret_type.clone(),
                kind: hir::ExprKind::RustFun(rust_fun.clone()),
            },
        };

        defs.push(def);
        inferred_locals.insert(var_id.local_id(), arret_type);
        exports.insert(fun_name_data_str, hir::scope::Binding::Var(var_id));
    }

    Ok(Module {
        module_id,

        imports: HashSet::new(),
        defs,
        inferred_locals: Arc::new(inferred_locals),
        exports: Arc::new(exports),

        main_var_id: None,
        rfi_library: Some(Arc::new(rfi_library)),
    })
}

/// Shared context for compilation
///
/// This isn't specific to a given program or REPL session. It acts as a global cache of compiled
/// source files and Rust libraries; it should be reused whenever possible.
pub struct CompileCtx {
    package_paths: PackagePaths,
    enable_optimisations: bool,

    source_loader: SourceLoader,
    rfi_loader: rfi::Loader,

    modules_by_name: PromiseMap<ModuleName, CachedModule>,
}

impl CompileCtx {
    pub fn new(package_paths: PackagePaths, enable_optimisations: bool) -> Self {
        use crate::hir::exports;
        use std::iter;

        let mut modules_by_name = PromiseMap::<ModuleName, CachedModule>::new();

        for (terminal_name, exports) in iter::once(("primitives", exports::prims_exports()))
            .chain(iter::once(("types", exports::tys_exports())))
        {
            // These modules are always loaded
            let prims_module = prims_to_module(exports);

            modules_by_name.insert(
                ModuleName::new(
                    "arret".into(),
                    vec!["internal".into()],
                    (*terminal_name).into(),
                ),
                prims_module.map(Arc::new),
            );
        }

        Self {
            package_paths,
            enable_optimisations,

            source_loader: SourceLoader::new(),
            rfi_loader: rfi::Loader::new(),
            modules_by_name,
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

    /// Returns a module for the given module name
    ///
    /// This returns a cached module; the module will only be compiled once per `CompileCtx`
    /// instance. If the module is being compiled on another thread this will block until the
    /// compilation is finished.
    fn get_module_by_name(&self, span: Span, module_name: ModuleName) -> CachedModule {
        self.modules_by_name
            .get_or_insert_with(
                module_name.clone(),
                move || match hir::loader::load_module_by_name(self, span, &module_name) {
                    Ok(LoadedModule::Source(source_file)) => {
                        self.source_file_to_module(&source_file).map(Arc::new)
                    }
                    Ok(LoadedModule::Rust(rfi_library)) => {
                        rfi_library_to_module(span, rfi_library).map(Arc::new)
                    }
                    Err(err) => Err(vec![err.into()]),
                },
            )
    }

    /// Returns an uncached module for a source file
    pub(crate) fn source_file_to_module(&self, source_file: &SourceFile) -> UncachedModule {
        let data = source_file
            .parsed()
            .map_err(|err| vec![diagnostic_for_syntax_error(&err)])?;

        self.data_to_module(data)
    }

    /// Collects all imports for a module's syntax data
    ///
    /// This attempts to import modules concurrently where possible
    pub(crate) fn imports_for_data<'a>(
        &self,
        data: impl Iterator<Item = &'a Datum>,
    ) -> Result<ModuleImports, Vec<Diagnostic<FileId>>> {
        let imported_module_names =
            import::collect_imported_module_names(data).map_err(errors_to_diagnostics)?;
        let import_count = imported_module_names.len();

        let loaded_module_results: Vec<(ModuleName, CachedModule)> = imported_module_names
            .into_iter()
            .map(|(module_name, span)| {
                let module = self.get_module_by_name(span, module_name.clone());
                (module_name, module)
            })
            .collect();

        let mut diagnostics = Vec::<Diagnostic<FileId>>::new();

        let mut imports = HashSet::<Arc<Module>>::with_capacity(import_count);
        let mut imported_exports = HashMap::<ModuleName, Arc<Exports>>::with_capacity(import_count);

        for (module_name, loaded_module_result) in loaded_module_results {
            match loaded_module_result {
                Ok(module) => {
                    imported_exports.insert(module_name, module.exports.clone());
                    imports.insert(module);
                }
                Err(mut new_diagnostics) => diagnostics.append(&mut new_diagnostics),
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }

        Ok(ModuleImports {
            imports,
            imported_exports,
        })
    }

    /// Returns an uncached module for syntax data
    pub(crate) fn data_to_module(&self, data: &[Datum]) -> UncachedModule {
        let ModuleImports {
            imports,
            imported_exports,
        } = self.imports_for_data(data.iter())?;

        let module_id = ModuleId::alloc();

        let lowered_module = hir::lowering::lower_data(module_id, &imported_exports, data)
            .map_err(errors_to_diagnostics)?;

        let LoweredModule {
            defs: lowered_defs,
            exports,
            main_var_id,
        } = lowered_module;

        let imported_inferred_vars = transitive_deps(&imports)
            .into_iter()
            .map(|module| (module.module_id, module.inferred_locals.clone()))
            .collect();

        let inferred_module = infer::infer_module(&imported_inferred_vars, module_id, lowered_defs)
            .map_err(errors_to_diagnostics)?;

        let infer::InferredModule {
            defs: inferred_defs,
            inferred_locals,
        } = inferred_module;

        Ok(Module {
            module_id,

            imports,
            defs: inferred_defs,
            inferred_locals: Arc::new(inferred_locals),
            exports: Arc::new(exports),
            main_var_id,

            rfi_library: None,
        })
    }
}
