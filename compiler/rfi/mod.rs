use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::{fmt, path};

use libloading;

use syntax::datum::Datum;
use syntax::span::Span;

use crate::hir;
use crate::hir::error::{Error, ErrorKind};
use crate::hir::ns::{NsDatum, NsId};
use crate::hir::scope::Scope;
use crate::source::{RfiModuleKind, SourceKind, SourceLoader};
use crate::ty;

use runtime::{abitype, binding};

pub struct Library {
    _loaded: libloading::Library,
    target_path: Box<path::Path>,
    exported_funs: HashMap<&'static str, Fun>,
}

impl Library {
    pub fn target_path(&self) -> &path::Path {
        &self.target_path
    }

    pub fn exported_funs(&self) -> &HashMap<&'static str, Fun> {
        &self.exported_funs
    }
}

impl fmt::Debug for Library {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rfi::Library({})", self.target_path.to_string_lossy())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    /// Name of this function if it corresponds to an intrinsic
    ///
    /// Intrinsics may have optimised partial evaluation in MIR. However, they should be
    /// semantically equivalent to the non-intrinsic version.
    intrinsic_name: Option<&'static str>,

    arret_fun_type: ty::Fun,
    takes_task: bool,
    params: &'static [abitype::ParamABIType],
    ret: &'static abitype::RetABIType,
    symbol: &'static str,
    entry_point: usize,
}

impl Fun {
    pub fn intrinsic_name(&self) -> Option<&'static str> {
        self.intrinsic_name
    }

    pub fn arret_fun_type(&self) -> &ty::Fun {
        &self.arret_fun_type
    }

    pub fn symbol(&self) -> &'static str {
        self.symbol
    }

    pub fn entry_point(&self) -> usize {
        self.entry_point
    }

    pub fn takes_task(&self) -> bool {
        self.takes_task
    }

    pub fn params(&self) -> &'static [abitype::ParamABIType] {
        self.params
    }

    pub fn has_rest(&self) -> bool {
        !self.arret_fun_type.params().rest().is_never()
    }

    pub fn ret(&self) -> &'static abitype::RetABIType {
        self.ret
    }
}

pub struct Loader {
    type_ns_id: NsId,
    type_scope: Scope,
    native_rust_libraries: Mutex<HashMap<Box<path::Path>, Arc<Library>>>,
}

/// Ensure that the specified Arret type is compatible with the corresponding Rust type
///
/// The Arret types are strictly more expressive than the Rust types. This simply checks that the
/// Arret type is more specific than the Rust type.
fn ensure_types_compatible<T>(
    span: Span,
    arret_poly: &ty::Ref<ty::Poly>,
    abi_type: &T,
) -> Result<(), Error>
where
    T: ty::conv_abi::ConvertableABIType,
{
    if ty::is_a::ty_ref_is_a(arret_poly, &abi_type.to_ty_ref()) {
        Ok(())
    } else {
        Err(Error::new(
            span,
            ErrorKind::RustFunError(
                format!(
                    "Rust type `{}` does not match declared Arret type of `{}`",
                    abi_type.to_rust_str(),
                    hir::str_for_ty_ref(arret_poly),
                )
                .into_boxed_str(),
            ),
        ))
    }
}

#[derive(Clone, Copy)]
enum LibType {
    Static,
    Dynamic,
}

fn build_rfi_lib_path(base: &path::Path, package_name: &str, lib_type: LibType) -> path::PathBuf {
    let mut path_buf = path::PathBuf::new();
    path_buf.push(base);

    #[cfg(debug_assertions)]
    path_buf.push("debug");

    #[cfg(not(debug_assertions))]
    path_buf.push("release");

    match lib_type {
        LibType::Dynamic => {
            #[cfg(any(target_os = "macos", target_os = "ios"))]
            path_buf.push(format!("lib{}.dylib", package_name));

            #[cfg(all(not(target_os = "macos"), not(target_os = "ios"),))]
            path_buf.push(format!("lib{}.so", package_name));
        }
        LibType::Static => {
            path_buf.push(format!("lib{}.a", package_name));
        }
    }

    path_buf
}

impl Loader {
    pub fn new() -> Loader {
        Loader {
            type_ns_id: Scope::root_ns_id(),
            type_scope: Scope::new_with_primitives(),
            native_rust_libraries: Mutex::new(HashMap::new()),
        }
    }

    fn process_rust_fun(
        &self,
        arret_type_datum: &Datum,
        entry_point: usize,
        rust_fun: &'static binding::RustFun,
        intrinsic_name: Option<&'static str>,
    ) -> Result<Fun, Error> {
        let ns_datum = NsDatum::from_syntax_datum(self.type_ns_id, arret_type_datum);
        let span = ns_datum.span();

        // Lower the Arret type using a fixed scope
        let poly_type = hir::lower_poly(&self.type_scope, ns_datum)?;

        // Ensure the type is actually a function type
        let poly_fun_type = if let ty::Ref::Fixed(ty::Ty::Fun(fun_type)) = poly_type {
            fun_type
        } else {
            return Err(Error::new(
                span,
                ErrorKind::RustFunError("function type expected".into()),
            ));
        };

        let pvar_ids = poly_fun_type.pvar_ids();
        let tvar_ids = poly_fun_type.tvar_ids();

        // The Rust function signature should satisfy the upper bound of the Arret type
        let pta = ty::ty_args::TyArgs::from_upper_bound(pvar_ids, tvar_ids);
        let upper_fun_type = ty::subst::subst_poly_fun(&pta, &*poly_fun_type);

        // Calculate how many parameters the Rust function should accept
        let expected_rust_params = upper_fun_type.params().fixed().len()
            + !upper_fun_type.params().rest().is_never() as usize;

        if expected_rust_params != rust_fun.params.len() {
            return Err(Error::new(
                span,
                ErrorKind::RustFunError(
                    format!(
                        "expected Rust function to have {} parameters; has {}",
                        expected_rust_params,
                        rust_fun.params.len()
                    )
                    .into_boxed_str(),
                ),
            ));
        }

        let mut abi_params_iter = rust_fun.params.iter();

        // If there are rest types ensure they're compatible
        let arret_rest = upper_fun_type.params().rest();
        if !arret_rest.is_never() {
            use runtime::abitype::{ABIType, BoxedABIType};

            let last_rust_param = abi_params_iter.next_back().unwrap();

            if let ABIType::Boxed(BoxedABIType::List(elem)) = &last_rust_param.abi_type {
                ensure_types_compatible(span, arret_rest, *elem)?;
            } else {
                return Err(Error::new(
                    span,
                    ErrorKind::RustFunError("expected Rust function to have `boxed::List` as last parameter to receive the rest argument".into())
                ));
            }
        };

        // Ensure the fixed types are compatible
        for (arret_fixed_poly, rust_fixed_poly) in
            upper_fun_type.params().fixed().iter().zip(abi_params_iter)
        {
            ensure_types_compatible(span, arret_fixed_poly, &rust_fixed_poly.abi_type)?;
        }

        // And the return type
        //
        // Note that we don't care about contravariance here; simply that the types are compatible
        ensure_types_compatible(span, upper_fun_type.ret(), &rust_fun.ret)?;

        Ok(Fun {
            intrinsic_name,

            arret_fun_type: *poly_fun_type,
            takes_task: rust_fun.takes_task,
            params: rust_fun.params,
            ret: &rust_fun.ret,
            symbol: rust_fun.symbol,
            entry_point,
        })
    }

    pub fn load(
        &self,
        span: Span,
        source_loader: &SourceLoader,
        native_base_path: &path::Path,
        target_base_path: &path::Path,
        package_name: &str,
    ) -> Result<Arc<Library>, Error> {
        let mut native_rust_libraries = self.native_rust_libraries.lock().unwrap();
        let native_path = build_rfi_lib_path(native_base_path, package_name, LibType::Dynamic);

        if let Some(library) = native_rust_libraries.get(native_path.as_path()) {
            return Ok(library.clone());
        }

        let target_path = build_rfi_lib_path(target_base_path, package_name, LibType::Static);

        let map_io_err = |err| Error::from_module_io(span, &native_path, &err);
        let loaded = libloading::Library::new(&native_path).map_err(map_io_err)?;

        let exports_symbol_name = format!("ARRET_{}_RUST_EXPORTS", package_name.to_uppercase());
        let exports: binding::RustExports = unsafe {
            let exports_symbol = loaded
                .get::<*const binding::RustExports>(exports_symbol_name.as_bytes())
                .map_err(map_io_err)?;

            &(**exports_symbol)
        };

        let filename: Arc<path::Path> = native_path.clone().into();
        let exported_funs = exports
            .iter()
            .map(|(fun_name, rust_fun)| {
                let entry_point_address = unsafe {
                    *loaded
                        .get::<usize>(rust_fun.symbol.as_bytes())
                        .map_err(map_io_err)?
                };

                // Parse the declared Arret type string as a datum
                let kind = SourceKind::RfiModule(RfiModuleKind {
                    filename: filename.clone(),
                    fun_name,
                });

                let arret_type_source_file =
                    source_loader.load_string(kind, rust_fun.arret_type.into());

                let arret_type_datum = match arret_type_source_file.parsed()? {
                    [arret_type_datum] => arret_type_datum,
                    _ => {
                        return Err(Error::new(
                            arret_type_source_file.span(),
                            ErrorKind::RustFunError("expected exactly one Arret type datum".into()),
                        ));
                    }
                };

                // Treat every native function in the stdlib as an intrinsic
                let intrinsic_name = Some(*fun_name).filter(|_| package_name == "stdlib");

                let fun = self.process_rust_fun(
                    arret_type_datum,
                    entry_point_address,
                    rust_fun,
                    intrinsic_name,
                )?;

                Ok((*fun_name, fun))
            })
            .collect::<Result<HashMap<&'static str, Fun>, Error>>()?;

        let library = Arc::new(Library {
            _loaded: loaded,
            target_path: target_path.into_boxed_path(),
            exported_funs,
        });

        native_rust_libraries.insert(native_path.into(), library.clone());

        Ok(library)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use runtime::abitype::{ABIType, BoxedABIType, ParamABIType, ParamCapture, RetABIType};
    use runtime::boxed::TypeTag;
    use syntax::parser::datum_from_str;

    fn binding_fun_to_poly_type(rust_fun: &'static binding::RustFun) -> Result<ty::Fun, Error> {
        let loader = Loader::new();
        let arret_type_datum = datum_from_str(rust_fun.arret_type).unwrap();

        loader
            .process_rust_fun(&arret_type_datum, 0, rust_fun, None)
            .map(|rfi_fun| rfi_fun.arret_fun_type)
    }

    fn assert_valid_binding_fun(rust_fun: &'static binding::RustFun) {
        binding_fun_to_poly_type(rust_fun).unwrap();
    }

    fn assert_binding_fun_error(expected_kind: &ErrorKind, rust_fun: &'static binding::RustFun) {
        assert_eq!(
            expected_kind,
            binding_fun_to_poly_type(rust_fun).unwrap_err().kind()
        );
    }

    #[test]
    fn exact_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Int -> Int)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int))),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn inexact_rust_fun_with_rest() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(& Int -> false)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::List(&BoxedABIType::DirectTagged(
                    TypeTag::Int,
                ))),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Bool),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn void_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Float -> '())",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Float,
                capture: ParamCapture::Never,
            }],
            ret: RetABIType::Void,
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn diverging_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(-> (U))",
            takes_task: false,
            params: &[],
            ret: RetABIType::Never,
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn polymorphic_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(All #{A} (List A & Any) -> A)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::Pair(&BoxedABIType::Any)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Any)),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn incompatible_polymorphic_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(All #{A} (List & Any) -> A)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::Pair(&BoxedABIType::Any)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Any)),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError(
            "Rust type `Gc<boxed::Pair<boxed::Any>>` does not match declared Arret type of `(List & Any)`".into(),
        );
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn unbound_arret_type() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(unbound)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Bool),
            symbol: "",
        };

        let kind = ErrorKind::UnboundSym("unbound".into());
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn non_fun_type() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "Str",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Bool),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError("function type expected".into());
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn non_list_rust_rest_param() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(& Int -> true)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Bool),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError("expected Rust function to have `boxed::List` as last parameter to receive the rest argument".into());
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn mismatched_fixed_param_count() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Int -> Int)",
            takes_task: false,
            params: &[
                ParamABIType {
                    abi_type: ABIType::Int,
                    capture: ParamCapture::Never,
                },
                ParamABIType {
                    abi_type: ABIType::Int,
                    capture: ParamCapture::Never,
                },
            ],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int))),
            symbol: "",
        };

        let kind =
            ErrorKind::RustFunError("expected Rust function to have 1 parameters; has 2".into());
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn incompatible_fixed_param_type() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Char -> Int)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int))),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError(
            "Rust type `i64` does not match declared Arret type of `Char`".into(),
        );
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn incompatible_ret_type() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Int -> Char)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::DirectTagged(TypeTag::Int))),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError(
            "Rust type `Gc<boxed::Int>` does not match declared Arret type of `Char`".into(),
        );
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }
}
