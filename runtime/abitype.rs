use crate::binding::Never;
use crate::boxed;
use crate::boxed::refs;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoxedABIType {
    Any,
    DirectTagged(boxed::TypeTag),
    Union(&'static str, &'static [boxed::TypeTag]),
    Vector(&'static BoxedABIType),
    List(&'static BoxedABIType),
    Pair(&'static BoxedABIType),
}

pub const TOP_LIST_BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::List(&BoxedABIType::Any);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ABIType {
    Bool,
    Char,
    Float,
    Int,
    Boxed(BoxedABIType),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParamCapture {
    /// Indicates the capture of this parameter should be automatically inferred
    Auto,
    /// Explicitly specifies that this parameter is never captured
    Never,
    /// Explicitly specifies that this parameter is captured
    Always,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParamABIType {
    pub abi_type: ABIType,
    pub capture: ParamCapture,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RetABIType {
    Void,
    Never,
    Inhabited(ABIType),
}

pub trait EncodeABIType {
    const ABI_TYPE: ABIType;
    /// Controls the capture type when this is used as a parameter
    const PARAM_CAPTURE: ParamCapture = ParamCapture::Never;
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
        type_tag.into_boxed_abi_type()
    }
}

impl BoxedABIType {
    pub fn into_abi_type(self) -> ABIType {
        ABIType::Boxed(self)
    }

    pub fn into_param_abi_type(self) -> ParamABIType {
        ParamABIType {
            abi_type: self.into_abi_type(),
            capture: ParamCapture::Auto,
        }
    }
}

impl From<boxed::TypeTag> for ABIType {
    fn from(type_tag: boxed::TypeTag) -> ABIType {
        type_tag.into_boxed_abi_type().into_abi_type()
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
}

impl From<boxed::TypeTag> for ParamABIType {
    fn from(type_tag: boxed::TypeTag) -> ParamABIType {
        type_tag.into_boxed_abi_type().into_param_abi_type()
    }
}
impl From<BoxedABIType> for ParamABIType {
    fn from(boxed_abi_type: BoxedABIType) -> ParamABIType {
        boxed_abi_type.into_param_abi_type()
    }
}

impl From<boxed::TypeTag> for RetABIType {
    fn from(type_tag: boxed::TypeTag) -> RetABIType {
        type_tag
            .into_boxed_abi_type()
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
