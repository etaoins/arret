use std::ffi::OsString;
use std::sync::Arc;
use std::{fmt, path};

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

use crate::hir;
use crate::hir::error::{Error, ErrorKind};
use crate::hir::ns::NsDatum;
use crate::hir::scope::Scope;
use crate::source::SourceLoader;
use crate::ty;
use crate::ty::Ty;

use arret_runtime::{abitype, binding};

pub struct Library {
    pub loaded: libloading::Library,
    pub target_path: Box<path::Path>,
    pub exported_funs: Box<[(&'static str, Arc<Fun>)]>,
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

    span: Span,
    arret_fun_type: ty::Fun,
    takes_task: bool,
    params: &'static [abitype::ParamAbiType],
    ret: &'static abitype::RetAbiType,
    symbol: &'static str,
    entry_point: usize,
}

impl Fun {
    pub fn intrinsic_name(&self) -> Option<&'static str> {
        self.intrinsic_name
    }

    pub fn span(&self) -> Span {
        self.span
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

    pub fn params(&self) -> &'static [abitype::ParamAbiType] {
        self.params
    }

    pub fn has_rest(&self) -> bool {
        self.arret_fun_type.params().has_rest()
    }

    pub fn ret(&self) -> &'static abitype::RetAbiType {
        self.ret
    }
}

pub struct Loader {
    type_scope: Scope<'static>,
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
    T: ty::conv_abi::ConvertableAbiType,
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
            type_scope: Scope::new_with_primitives(),
        }
    }

    fn process_rust_fun(
        &self,
        arret_type_datum: &Datum,
        entry_point: usize,
        rust_fun: &'static binding::RustFun,
        intrinsic_name: Option<&'static str>,
    ) -> Result<Fun, Error> {
        let ns_datum = NsDatum::from_syntax_datum(arret_type_datum);
        let span = ns_datum.span();

        // Lower the Arret type using a fixed scope
        let poly_type = hir::lower_poly(&self.type_scope, ns_datum)?;

        // Ensure the type is actually a function type
        let poly_fun_type = if let ty::Ref::Fixed(Ty::Fun(fun_type)) = poly_type {
            fun_type
        } else {
            return Err(Error::new(
                span,
                ErrorKind::RustFunError("function type expected".into()),
            ));
        };

        let pvars = poly_fun_type.pvars();
        let tvars = poly_fun_type.tvars();

        // The Rust function signature should satisfy the upper bound of the Arret type
        let pta = ty::ty_args::TyArgs::from_upper_bound(pvars, tvars);
        let upper_fun_type = ty::subst::subst_poly_fun(&pta, &*poly_fun_type);

        // Calculate how many parameters the Rust function should accept
        let expected_rust_params =
            upper_fun_type.params().fixed().len() + upper_fun_type.params().has_rest() as usize;

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
            use arret_runtime::abitype::{AbiType, BoxedAbiType};

            let last_rust_param = abi_params_iter.next_back().unwrap();

            if let AbiType::Boxed(BoxedAbiType::List(elem)) = &last_rust_param.abi_type {
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

            span: arret_type_datum.span(),
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
    ) -> Result<Library, Error> {
        let native_path = build_rfi_lib_path(native_base_path, package_name, LibType::Dynamic);
        let target_path = build_rfi_lib_path(target_base_path, package_name, LibType::Static);

        let map_loader_err = |err: libloading::Error| match err {
            libloading::Error::DlOpen { .. } | libloading::Error::DlOpenUnknown => Error::new(
                span,
                ErrorKind::ModuleNotFound(native_path.clone().into_boxed_path()),
            ),
            _ => Error::new(
                span,
                ErrorKind::ReadError(native_path.clone().into_boxed_path()),
            ),
        };

        let loaded = unsafe { libloading::Library::new(&native_path).map_err(map_loader_err)? };

        let exports_symbol_name = format!("ARRET_{}_RUST_EXPORTS", package_name.to_uppercase());
        let exports: binding::RustExports = unsafe {
            let exports_symbol = loaded
                .get::<*const binding::RustExports>(exports_symbol_name.as_bytes())
                .map_err(map_loader_err)?;

            **exports_symbol
        };

        source_loader.reserve(exports.len());

        let exported_funs = exports
            .iter()
            .map(|(fun_name, rust_fun)| {
                let entry_point_address = unsafe {
                    *loaded
                        .get::<usize>(rust_fun.symbol.as_bytes())
                        .map_err(map_loader_err)?
                };

                // Parse the declared Arret type string as a datum
                let mut file_map_name =
                    OsString::with_capacity(native_path.as_os_str().len() + 1 + fun_name.len());

                file_map_name.push(native_path.as_os_str());
                file_map_name.push(":");
                file_map_name.push(fun_name);

                let arret_type_source_file =
                    source_loader.load_string(file_map_name, rust_fun.arret_type);

                let arret_type_datum = match arret_type_source_file.parsed()? {
                    [arret_type_datum] => arret_type_datum,
                    _ => {
                        return Err(Error::new(
                            Span::from_str(
                                Some(arret_type_source_file.file_id()),
                                rust_fun.arret_type,
                            ),
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

                Ok((*fun_name, Arc::new(fun)))
            })
            .collect::<Result<Box<[(&'static str, Arc<Fun>)]>, Error>>()?;

        Ok(Library {
            loaded,
            target_path: target_path.into_boxed_path(),
            exported_funs,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use arret_runtime::abitype::{AbiType, BoxedAbiType, ParamAbiType, ParamCapture, RetAbiType};
    use arret_runtime::boxed::TypeTag;
    use arret_syntax::parser::datum_from_str;

    fn binding_fun_to_poly_type(rust_fun: &'static binding::RustFun) -> Result<ty::Fun, Error> {
        let loader = Loader::new();
        let arret_type_datum = datum_from_str(None, rust_fun.arret_type).unwrap();

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
            params: &[ParamAbiType {
                abi_type: AbiType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int))),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn inexact_rust_fun_with_rest() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(& Int -> false)",
            takes_task: false,
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::List(&BoxedAbiType::UniqueTagged(
                    TypeTag::Int,
                ))),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Bool),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn void_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(Float -> '())",
            takes_task: false,
            params: &[ParamAbiType {
                abi_type: AbiType::Float,
                capture: ParamCapture::Never,
            }],
            ret: RetAbiType::Void,
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
            ret: RetAbiType::Never,
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn polymorphic_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(All #{A} (List A & Any) -> A)",
            takes_task: false,
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::Pair(&BoxedAbiType::Any)),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::Any)),
            symbol: "",
        };

        assert_valid_binding_fun(&BINDING_RUST_FUN);
    }

    #[test]
    fn incompatible_polymorphic_rust_fun() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "(All #{A} (List & Any) -> A)",
            takes_task: false,
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::Pair(&BoxedAbiType::Any)),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::Any)),
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
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Bool),
            symbol: "",
        };

        let kind = ErrorKind::UnboundIdent("unbound".into());
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }

    #[test]
    fn non_fun_type() {
        const BINDING_RUST_FUN: binding::RustFun = binding::RustFun {
            arret_type: "Str",
            takes_task: false,
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Bool),
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
            params: &[ParamAbiType {
                abi_type: AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int)),
                capture: ParamCapture::Auto,
            }],
            ret: RetAbiType::Inhabited(AbiType::Bool),
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
                ParamAbiType {
                    abi_type: AbiType::Int,
                    capture: ParamCapture::Never,
                },
                ParamAbiType {
                    abi_type: AbiType::Int,
                    capture: ParamCapture::Never,
                },
            ],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int))),
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
            params: &[ParamAbiType {
                abi_type: AbiType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int))),
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
            params: &[ParamAbiType {
                abi_type: AbiType::Int,
                capture: ParamCapture::Never,
            }],
            ret: RetAbiType::Inhabited(AbiType::Boxed(BoxedAbiType::UniqueTagged(TypeTag::Int))),
            symbol: "",
        };

        let kind = ErrorKind::RustFunError(
            "Rust type `Gc<boxed::Int>` does not match declared Arret type of `Char`".into(),
        );
        assert_binding_fun_error(&kind, &BINDING_RUST_FUN);
    }
}
