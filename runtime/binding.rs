//! Macros and types for defining Rust RFI modules

use crate::abitype::{ParamABIType, RetABIType};

#[allow(clippy::useless_attribute)]
#[allow(unused)]
use crate::abitype::{EncodeABIType, EncodeRetABIType};

#[derive(Debug)]
pub struct RustFun {
    pub arret_type: &'static str,
    pub takes_task: bool,
    pub params: &'static [ParamABIType],
    pub ret: RetABIType,
    pub symbol: &'static str,
}

pub type RustExports = &'static [(&'static str, &'static RustFun)];

// TODO: Replace with ! once it's stable
pub enum Never {}

/// Defines a new Arret module implemented at Rust
///
/// Each Arret package can have an optional Rust module accessible as `(import [package-name
/// rust])`. These are loaded both at compile-time to support constant evaluation and linked against
/// compiled programs.
///
/// The first argument should be an identifier in the form of `ARRET_{PACKAGE_NAME}_RUST_EXPORTS`
/// where `{PACKAGE_NAME}` is the uppercased name of the package. For example, the package `stdlib`
/// uses `ARRET_STDLIB_RUST_EXPORTS`. This must be unique to prevent symbol conflicts when loading
/// Rust modules.
///
/// The second argument is a mapping of export names to Rust functions. These are defined using
/// the `rfi_derive::rust_fun` attribute macro.
#[macro_export]
macro_rules! define_rust_module {
    ($exports_sym:ident, { $( $export_name:expr => $desc_name:ident ),* }) => {
        #[no_mangle]
        pub static $exports_sym: RustExports = &[
            $(
                ($export_name, &$desc_name)
            ),*
        ];
    };
}
