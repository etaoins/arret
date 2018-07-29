use std::path;

use libloading;

use runtime::binding;

use hir::error::Error;
use hir::module::Module;
use syntax::span::Span;

fn push_rfi_lib_path(path_buf: &mut path::PathBuf, package_name: &str) {
    #[cfg(debug_assertions)]
    path_buf.push("debug");

    #[cfg(not(debug_assertions))]
    path_buf.push("release");

    #[cfg(any(target_os = "macos", target_os = "ios"))]
    path_buf.push(format!("lib{}.dylib", package_name));

    #[cfg(any(target_os = "windows"))]
    path_buf.push(format!("{}.dll", package_name));

    #[cfg(
        all(
            not(target_os = "macos"),
            not(target_os = "ios"),
            not(target_os = "windows")
        )
    )]
    path_buf.push(format!("lib{}.so", package_name));
}

pub fn load_rfi_module(
    span: Span,
    base_path: &path::Path,
    package_name: &str,
) -> Result<Module, Error> {
    let mut path_buf = path::PathBuf::new();
    path_buf.push(base_path);
    push_rfi_lib_path(&mut path_buf, package_name);

    let path = path_buf.as_path();
    let map_io_err = |err| Error::from_module_io(span, path, &err);

    let rfi_lib = libloading::Library::new(path).map_err(map_io_err)?;

    let exports: binding::RustExports = unsafe {
        let exports_symbol = rfi_lib
            .get::<*const binding::RustExports>(b"ARRET_RUST_EXPORTS")
            .map_err(map_io_err)?;

        &(**exports_symbol)
    };

    unimplemented!("Load RFI {:?}", exports.len());
}
