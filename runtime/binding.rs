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
