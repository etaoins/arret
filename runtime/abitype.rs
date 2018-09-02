use crate::binding::Never;
use crate::boxed;
use crate::boxed::refs::Gc;
use crate::intern::InternedSym;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoxedABIType {
    Any,
    DirectTagged(boxed::TypeTag),
    Union(&'static str, &'static [boxed::TypeTag]),
    Vector(&'static BoxedABIType),
    List(&'static BoxedABIType),
    Pair(&'static BoxedABIType),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ABIType {
    Bool,
    Char,
    Float,
    Int,
    InternedSym,
    Boxed(BoxedABIType),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RetABIType {
    Void,
    Never,
    Inhabited(ABIType),
}

pub trait EncodeABIType {
    const ABI_TYPE: ABIType;
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

impl EncodeABIType for InternedSym {
    const ABI_TYPE: ABIType = ABIType::InternedSym;
}

impl<T: boxed::Boxed> EncodeABIType for Gc<T>
where
    T: EncodeBoxedABIType,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
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
