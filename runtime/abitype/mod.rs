use boxed;
use boxed::refs::Gc;
use intern::InternedSym;

#[derive(Debug)]
pub enum BoxedABIType {
    Any,
    DirectTagged(boxed::TypeTag),
    Union(&'static [boxed::TypeTag]),
    Vector(&'static BoxedABIType),
    List(&'static BoxedABIType),
    Pair(&'static BoxedABIType),
}

#[derive(Debug)]
pub enum ABIType {
    Bool,
    Char,
    Float,
    Int,
    InternedSym,
    Void,
    Boxed(BoxedABIType),
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

impl EncodeABIType for () {
    const ABI_TYPE: ABIType = ABIType::Void;
}

impl EncodeABIType for InternedSym {
    const ABI_TYPE: ABIType = ABIType::InternedSym;
}

impl<T> EncodeABIType for Gc<T>
where
    T: EncodeBoxedABIType + boxed::Boxed,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
}

pub trait EncodeBoxedABIType {
    const BOXED_ABI_TYPE: BoxedABIType;
}
