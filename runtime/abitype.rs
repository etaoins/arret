//! Type encoding for Rust types
//!
//! This is a system of traits used to encode Rust types in a form understandable by the Arret
//! compiler. It's used to ensure type safety across the RFI boundary.

use crate::binding::Never;
use crate::boxed;
use crate::boxed::refs;
use crate::callback;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoxedABIType {
    Any,
    UniqueTagged(boxed::TypeTag),
    Union(&'static str, &'static [boxed::TypeTag]),
    Vector(&'static BoxedABIType),
    List(&'static BoxedABIType),
    Pair(&'static BoxedABIType),
    Set(&'static BoxedABIType),
    Map(&'static BoxedABIType, &'static BoxedABIType),
}

pub const TOP_LIST_BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::List(&BoxedABIType::Any);

/// Encoded type for any boxed or unboxed value
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ABIType {
    /// Unboxed boolean value
    ///
    /// This is identical to [`bool`] in Rust and C++
    Bool,

    /// Unboxed character value
    ///
    /// This is identical to [`char`] in Rust and `wchar_t` in C++
    Char,

    /// Unboxed 64bit float
    ///
    /// This is identical to [`f64`] in Rust and `double` in C++
    Float,

    /// Unboxed signed 64bit integer
    ///
    /// This is identical to in [`i64`] in Rust and `std::int64_t` in C++
    Int,

    /// Interned integer for a `Sym`
    ///
    /// While this corresponds to [`InternedSym`](crate::intern::InternedSym) it's currently only
    /// used internally by the compiler.
    InternedSym,

    /// [Boxed value](crate::boxed)
    Boxed(BoxedABIType),

    /// [Callback function](crate::callback)
    Callback(&'static callback::EntryPointABIType),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ParamCapture {
    /// Indicates the capture of this parameter should be automatically inferred
    Auto,
    /// Explicitly specifies that this parameter is never captured
    Never,
    /// Explicitly specifies that this parameter is captured
    Always,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ParamABIType {
    pub abi_type: ABIType,
    pub capture: ParamCapture,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RetABIType {
    Void,
    Never,
    Inhabited(ABIType),
}

pub trait EncodeABIType {
    const ABI_TYPE: ABIType;
    /// Controls the capture type when this is used as a parameter
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Never;

    const PARAM_ABI_TYPE: ParamABIType = ParamABIType {
        abi_type: Self::ABI_TYPE,
        capture: Self::PARAM_CAPTURE,
    };
}

impl EncodeABIType for f64 {
    const ABI_TYPE: ABIType = ABIType::Float;
}

impl EncodeABIType for i64 {
    const ABI_TYPE: ABIType = ABIType::Int;
}

impl EncodeABIType for char {
    const ABI_TYPE: ABIType = ABIType::Char;
}

impl EncodeABIType for bool {
    const ABI_TYPE: ABIType = ABIType::Bool;
}

impl<T: boxed::Boxed> EncodeABIType for refs::Gc<T>
where
    T: EncodeBoxedABIType,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Auto;
}

impl<T: boxed::Boxed> EncodeABIType for refs::NoCapture<T>
where
    T: EncodeBoxedABIType,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Never;
}

impl<T: boxed::Boxed> EncodeABIType for refs::Capture<T>
where
    T: EncodeBoxedABIType,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Always;
}

impl<F> EncodeABIType for callback::Callback<F>
where
    F: callback::EncodeEntryPointABIType,
{
    const ABI_TYPE: ABIType = ABIType::Callback(&F::ENTRY_POINT_ABI_TYPE);
}

pub trait EncodeBoxedABIType {
    const BOXED_ABI_TYPE: BoxedABIType;
}

pub trait EncodeRetABIType {
    const RET_ABI_TYPE: RetABIType;
}

impl<T: EncodeABIType> EncodeRetABIType for T {
    const RET_ABI_TYPE: RetABIType = RetABIType::Inhabited(Self::ABI_TYPE);
}

impl EncodeRetABIType for () {
    const RET_ABI_TYPE: RetABIType = RetABIType::Void;
}

impl EncodeRetABIType for Never {
    const RET_ABI_TYPE: RetABIType = RetABIType::Never;
}

impl From<boxed::TypeTag> for BoxedABIType {
    fn from(type_tag: boxed::TypeTag) -> BoxedABIType {
        type_tag.to_boxed_abi_type()
    }
}

impl BoxedABIType {
    pub fn into_abi_type(self) -> ABIType {
        ABIType::Boxed(self)
    }
}

impl From<boxed::TypeTag> for ABIType {
    fn from(type_tag: boxed::TypeTag) -> ABIType {
        type_tag.to_boxed_abi_type().into_abi_type()
    }
}

impl From<BoxedABIType> for ABIType {
    fn from(boxed_abi_type: BoxedABIType) -> ABIType {
        boxed_abi_type.into_abi_type()
    }
}

impl ABIType {
    pub fn into_ret_abi_type(self) -> RetABIType {
        RetABIType::Inhabited(self)
    }

    pub fn into_param_abi_type(self) -> ParamABIType {
        let capture = match self {
            ABIType::Boxed(_) => ParamCapture::Auto,
            _ => ParamCapture::Never,
        };

        ParamABIType {
            abi_type: self,
            capture,
        }
    }

    pub fn may_contain_gc_refs(&self) -> bool {
        match self {
            ABIType::Boxed(_) | ABIType::InternedSym | ABIType::Callback(_) => true,
            _ => false,
        }
    }
}

impl From<boxed::TypeTag> for ParamABIType {
    fn from(type_tag: boxed::TypeTag) -> ParamABIType {
        type_tag
            .to_boxed_abi_type()
            .into_abi_type()
            .into_param_abi_type()
    }
}

impl From<BoxedABIType> for ParamABIType {
    fn from(boxed_abi_type: BoxedABIType) -> ParamABIType {
        boxed_abi_type.into_abi_type().into_param_abi_type()
    }
}

impl From<ABIType> for ParamABIType {
    fn from(abi_type: ABIType) -> ParamABIType {
        abi_type.into_param_abi_type()
    }
}

impl From<boxed::TypeTag> for RetABIType {
    fn from(type_tag: boxed::TypeTag) -> RetABIType {
        type_tag
            .to_boxed_abi_type()
            .into_abi_type()
            .into_ret_abi_type()
    }
}
impl From<BoxedABIType> for RetABIType {
    fn from(boxed_abi_type: BoxedABIType) -> RetABIType {
        boxed_abi_type.into_abi_type().into_ret_abi_type()
    }
}

impl From<ABIType> for RetABIType {
    fn from(abi_type: ABIType) -> RetABIType {
        abi_type.into_ret_abi_type()
    }
}
