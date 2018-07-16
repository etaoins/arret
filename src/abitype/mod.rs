use boxed;
use boxed::refs::Gc;

#[derive(Debug)]
pub enum BoxedABIType {
    Direct(boxed::TypeTag),
    Vector(&'static BoxedABIType),
}

#[derive(Debug)]
pub enum ABIType {
    Bool,
    Char,
    Float,
    Int,
    Void,
    Boxed(BoxedABIType),
}

pub trait EncodeABIType {
    const ABI_TYPE: ABIType;
}

pub trait EncodeBoxedABIType {
    const BOXED_ABI_TYPE: BoxedABIType;
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

impl<T> EncodeABIType for Gc<T>
where
    T: EncodeBoxedABIType + boxed::Boxed,
{
    const ABI_TYPE: ABIType = ABIType::Boxed(T::BOXED_ABI_TYPE);
}

impl<T> EncodeBoxedABIType for T
where
    T: boxed::DirectTagged,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Direct(T::TYPE_TAG);
}

impl<T> EncodeBoxedABIType for boxed::Vector<T>
where
    T: EncodeBoxedABIType + boxed::Boxed,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Vector(&T::BOXED_ABI_TYPE);
}
