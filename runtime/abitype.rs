//! Type encoding for Rust types
//!
//! This is a system of traits used to encode Rust types in a form understandable by the Arret
//! compiler. It's used to ensure type safety across the RFI boundary.

use crate::binding::Never;
use crate::boxed;
use crate::boxed::refs;
use crate::callback;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoxedAbiType {
    Any,
    UniqueTagged(boxed::TypeTag),
    Union(&'static str, &'static [boxed::TypeTag]),
    Vector(&'static BoxedAbiType),
    List(&'static BoxedAbiType),
    Pair(&'static BoxedAbiType),
    Set(&'static BoxedAbiType),
    Map(&'static BoxedAbiType, &'static BoxedAbiType),
}

pub const TOP_LIST_BOXED_ABI_TYPE: BoxedAbiType = BoxedAbiType::List(&BoxedAbiType::Any);

/// Encoded type for any boxed or unboxed value
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum AbiType {
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
    Boxed(BoxedAbiType),

    /// [Callback function](crate::callback)
    Callback(&'static callback::EntryPointAbiType),
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
pub struct ParamAbiType {
    pub abi_type: AbiType,
    pub capture: ParamCapture,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RetAbiType {
    Void,
    Never,
    Inhabited(AbiType),
}

pub trait EncodeAbiType {
    const ABI_TYPE: AbiType;
    /// Controls the capture type when this is used as a parameter
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Never;

    const PARAM_ABI_TYPE: ParamAbiType = ParamAbiType {
        abi_type: Self::ABI_TYPE,
        capture: Self::PARAM_CAPTURE,
    };
}

impl EncodeAbiType for f64 {
    const ABI_TYPE: AbiType = AbiType::Float;
}

impl EncodeAbiType for i64 {
    const ABI_TYPE: AbiType = AbiType::Int;
}

impl EncodeAbiType for char {
    const ABI_TYPE: AbiType = AbiType::Char;
}

impl EncodeAbiType for bool {
    const ABI_TYPE: AbiType = AbiType::Bool;
}

impl<T: boxed::Boxed> EncodeAbiType for refs::Gc<T>
where
    T: EncodeBoxedAbiType,
{
    const ABI_TYPE: AbiType = AbiType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Auto;
}

impl<T: boxed::Boxed> EncodeAbiType for refs::NoCapture<T>
where
    T: EncodeBoxedAbiType,
{
    const ABI_TYPE: AbiType = AbiType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Never;
}

impl<T: boxed::Boxed> EncodeAbiType for refs::Capture<T>
where
    T: EncodeBoxedAbiType,
{
    const ABI_TYPE: AbiType = AbiType::Boxed(T::BOXED_ABI_TYPE);
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Always;
}

impl<F> EncodeAbiType for callback::Callback<F>
where
    F: callback::EncodeEntryPointAbiType,
{
    const ABI_TYPE: AbiType = AbiType::Callback(&F::ENTRY_POINT_ABI_TYPE);
}

pub trait EncodeBoxedAbiType {
    const BOXED_ABI_TYPE: BoxedAbiType;
}

pub trait EncodeRetAbiType {
    const RET_ABI_TYPE: RetAbiType;
}

impl<T: EncodeAbiType> EncodeRetAbiType for T {
    const RET_ABI_TYPE: RetAbiType = RetAbiType::Inhabited(Self::ABI_TYPE);
}

impl EncodeRetAbiType for () {
    const RET_ABI_TYPE: RetAbiType = RetAbiType::Void;
}

impl EncodeRetAbiType for Never {
    const RET_ABI_TYPE: RetAbiType = RetAbiType::Never;
}

impl From<boxed::TypeTag> for BoxedAbiType {
    fn from(type_tag: boxed::TypeTag) -> BoxedAbiType {
        type_tag.to_boxed_abi_type()
    }
}

impl BoxedAbiType {
    pub fn into_abi_type(self) -> AbiType {
        AbiType::Boxed(self)
    }
}

impl From<boxed::TypeTag> for AbiType {
    fn from(type_tag: boxed::TypeTag) -> AbiType {
        type_tag.to_boxed_abi_type().into_abi_type()
    }
}

impl From<BoxedAbiType> for AbiType {
    fn from(boxed_abi_type: BoxedAbiType) -> AbiType {
        boxed_abi_type.into_abi_type()
    }
}

impl AbiType {
    pub fn into_ret_abi_type(self) -> RetAbiType {
        RetAbiType::Inhabited(self)
    }

    pub fn into_param_abi_type(self) -> ParamAbiType {
        let capture = match self {
            AbiType::Boxed(_) => ParamCapture::Auto,
            _ => ParamCapture::Never,
        };

        ParamAbiType {
            abi_type: self,
            capture,
        }
    }

    pub fn may_contain_gc_refs(&self) -> bool {
        matches!(
            self,
            AbiType::Boxed(_) | AbiType::InternedSym | AbiType::Callback(_)
        )
    }
}

impl From<boxed::TypeTag> for ParamAbiType {
    fn from(type_tag: boxed::TypeTag) -> ParamAbiType {
        type_tag
            .to_boxed_abi_type()
            .into_abi_type()
            .into_param_abi_type()
    }
}

impl From<BoxedAbiType> for ParamAbiType {
    fn from(boxed_abi_type: BoxedAbiType) -> ParamAbiType {
        boxed_abi_type.into_abi_type().into_param_abi_type()
    }
}

impl From<AbiType> for ParamAbiType {
    fn from(abi_type: AbiType) -> ParamAbiType {
        abi_type.into_param_abi_type()
    }
}

impl From<boxed::TypeTag> for RetAbiType {
    fn from(type_tag: boxed::TypeTag) -> RetAbiType {
        type_tag
            .to_boxed_abi_type()
            .into_abi_type()
            .into_ret_abi_type()
    }
}
impl From<BoxedAbiType> for RetAbiType {
    fn from(boxed_abi_type: BoxedAbiType) -> RetAbiType {
        boxed_abi_type.into_abi_type().into_ret_abi_type()
    }
}

impl From<AbiType> for RetAbiType {
    fn from(abi_type: AbiType) -> RetAbiType {
        abi_type.into_ret_abi_type()
    }
}
