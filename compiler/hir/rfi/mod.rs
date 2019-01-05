use std::collections::HashMap;
use std::os::raw::c_void;
use std::path;

use libloading;

use syntax::span::Span;

use crate::hir::error::{Error, ErrorKind};
use crate::hir::ns::{NsDatum, NsId};
use crate::hir::scope::Scope;
use crate::hir::types;
use crate::source::{SourceKind, SourceLoader};
use crate::ty;
use crate::ty::purity;

use runtime::{abitype, binding};

new_indexing_id_type!(RustLibraryId, u32);

pub struct Library {
    _loaded: libloading::Library,
    target_path: Box<path::Path>,
}

impl Library {
    pub fn target_path(&self) -> &path::Path {
        &self.target_path
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    rust_library_id: RustLibraryId,

    /// Name of this function if it corresponds to an instrinsic
    ///
    /// Intrinsics may have optimized partial evaluation in MIR. However, they should be
    /// semantically equivalent to the non-instrinsic version.
    intrinsic_name: Option<&'static str>,

    arret_fun_type: ty::Fun,
    takes_task: bool,
    params: &'static [abitype::ParamABIType],
    ret: &'static abitype::RetABIType,
    // TODO: Use ! once its stable
    symbol: &'static str,
    entry_point: *const c_void,
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

    pub fn entry_point(&self) -> *const c_void {
        self.entry_point
    }

    pub fn takes_task(&self) -> bool {
        self.takes_task
    }

    pub fn params(&self) -> &'static [abitype::ParamABIType] {
        self.params
    }

    pub fn has_rest(&self) -> bool {
        self.arret_fun_type.params().rest().is_some()
    }

    pub fn ret(&self) -> &'static abitype::RetABIType {
        self.ret
    }
}

pub type Module = HashMap<&'static str, Fun>;

pub struct Loader {
    type_ns_id: NsId,
    type_scope: Scope,
    rust_libraries: Vec<Library>,
}

/// Ensure that the specified Arret type is compatible with the corresponding Rust type
///
/// The Arret types are strictly more expressive than the Rust types. This simply checks that the
/// Arret type is more specific than the Rust type.
fn ensure_types_compatible<T>(
    pvars: &purity::PVars,
    tvars: &ty::TVars,
    span: Span,
    arret_poly: &ty::Poly,
    abi_type: &T,
) -> Result<(), Error>
where
    T: ty::conv_abi::ConvertableABIType,
{
    if ty::is_a::ty_ref_is_a(tvars, arret_poly, &abi_type.to_ty_ref()).to_bool() {
        Ok(())
    } else {
        Err(Error::new(
            span,
            ErrorKind::RustFunError(
                format!(
                    "Rust type `{}` does not match declared Arret type of `{}`",
                    abi_type.to_rust_str(),
                    types::str_for_poly(pvars, tvars, arret_poly),
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
            rust_libraries: vec![],
        }
    }

    fn process_rust_fun(
        &self,
        rust_library_id: RustLibraryId,
        entry_point: *const c_void,
        rust_fun: &'static binding::RustFun,
        intrinsic_name: Option<&'static str>,
    ) -> Result<Fun, Error> {
        use syntax::parser::datum_from_str;

        // Parse the declared Arret type as a datum
        let syntax_datum = datum_from_str(rust_fun.arret_type)?;
        let ns_datum = NsDatum::from_syntax_datum(self.type_ns_id, syntax_datum);
        let span = ns_datum.span();

        // Lower the Arret type using a fixed scope
        let poly_type = types::lower_poly(&self.type_scope, ns_datum)?;

        // Ensure the type is actually a function type
        let poly_fun_type = if let ty::Poly::Fixed(ty::Ty::Fun(fun_type)) = poly_type {
            fun_type
        } else {
            return Err(Error::new(
                span,
                ErrorKind::RustFunError("function type expected".into()),
            ));
        };

        let pvars = poly_fun_type.pvars();
        let tvars = poly_fun_type.tvars();

        // The Rust function signature should match the upper bound of the Arret type
        let pta = ty::ty_args::PolyTyArgs::from_upper_bound(pvars, tvars);
        let upper_fun_type = ty::subst::subst_poly_fun(&pta, &*poly_fun_type);

        // Calculate how many parameters the Rust function should accept
        let expected_rust_params = upper_fun_type.params().fixed().len()
            + upper_fun_type.params().rest().is_some() as usize;

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
        if let Some(arret_rest) = upper_fun_type.params().rest() {
            use runtime::abitype::{ABIType, BoxedABIType};

            let last_rust_param = abi_params_iter.next_back().unwrap();

            if let ABIType::Boxed(BoxedABIType::List(elem)) = &last_rust_param.abi_type {
                ensure_types_compatible(pvars, tvars, span, arret_rest, *elem)?;
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
            ensure_types_compatible(
                pvars,
                tvars,
                span,
                arret_fixed_poly,
                &rust_fixed_poly.abi_type,
            )?;
        }

        // And the return type
        //
        // Note that we don't care about contravariance here; simply that the types are compatible
        ensure_types_compatible(pvars, tvars, span, upper_fun_type.ret(), &rust_fun.ret)?;

        Ok(Fun {
            rust_library_id,

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
        &mut self,
        span: Span,
        source_loader: &mut SourceLoader,
        native_base_path: &path::Path,
        target_base_path: &path::Path,
        package_name: &str,
    ) -> Result<HashMap<&'static str, Fun>, Error> {
        let native_path = build_rfi_lib_path(native_base_path, package_name, LibType::Dynamic);
        let target_path = build_rfi_lib_path(target_base_path, package_name, LibType::Static);

        let map_io_err = |err| Error::from_module_io(span, &native_path, &err);
        let loaded = libloading::Library::new(&native_path).map_err(map_io_err)?;

        let rust_library_id = RustLibraryId::new(self.rust_libraries.len());

        let exports_symbol_name = format!("ARRET_{}_RUST_EXPORTS", package_name.to_uppercase());
        let exports: binding::RustExports = unsafe {
            let exports_symbol = loaded
                .get::<*const binding::RustExports>(exports_symbol_name.as_bytes())
                .map_err(map_io_err)?;

            &(**exports_symbol)
        };

        let module = exports
            .iter()
            .map(|(name, rust_fun)| {
                let entry_point_address = unsafe {
                    *loaded
                        .get::<*const c_void>(rust_fun.symbol.as_bytes())
                        .map_err(map_io_err)?
                };

                // Treat every native function in the stdlib as an intrinsic
                let intrinsic_name = Some(*name).filter(|_| package_name == "stdlib");

                let fun = self
                    .process_rust_fun(
                        rust_library_id,
                        entry_point_address,
                        rust_fun,
                        intrinsic_name,
                    )
                    .map_err(|err| {
                        // This is a gross hack. We don't want to insert an entry in to the
                        // `SourceLoader` for every Rust function. This requires at least one memory
                        // allocation for the display name and extending the loaded sources. Instead
                        // only "load" the string once there is an error and adjust the span to match.
                        let kind = SourceKind::RfiModule(
                            native_path.to_string_lossy().into(),
                            (*name).to_owned(),
                        );
                        let error_offset = source_loader.next_span_offset;

                        source_loader.load_string(kind, rust_fun.arret_type.to_owned());

                        err.with_span_offset(error_offset)
                    })?;

                Ok((*name, fun))
            })
            .collect::<Result<HashMap<&'static str, Fun>, Error>>()?;

        self.rust_libraries.push(Library {
            _loaded: loaded,
            target_path: target_path.into_boxed_path(),
        });

        Ok(module)
    }

    pub fn into_rust_libraries(self) -> Vec<Library> {
        self.rust_libraries
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use runtime::abitype::{ABIType, BoxedABIType, ParamABIType, ParamCapture, RetABIType};
    use runtime::boxed::TypeTag;
    use std::ptr;

    fn binding_fun_to_poly_type(rust_fun: &'static binding::RustFun) -> Result<ty::Fun, Error> {
        let loader = Loader::new();

        loader
            .process_rust_fun(RustLibraryId::new(0), ptr::null(), rust_fun, None)
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
            arret_type: "(Int ... -> false)",
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
            arret_type: "(All #{A} (List A Any ...) -> A)",
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
            arret_type: "(All #{A} (List Any ...) -> A)",
            takes_task: false,
            params: &[ParamABIType {
                abi_type: ABIType::Boxed(BoxedABIType::Pair(&BoxedABIType::Any)),
                capture: ParamCapture::Auto,
            }],
            ret: RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Any)),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError(
            "Rust type `Gc<boxed::Pair<boxed::Any>>` does not match declared Arret type of `(Listof Any)`".into(),
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
            arret_type: "(Int ... -> true)",
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
