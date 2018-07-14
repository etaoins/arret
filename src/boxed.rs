#[derive(Debug)]
#[repr(u8)]
pub enum TypeTag {
    Bool,
    Char,
    Float,
    Int,
    List,
    Nil,
    Str,
    Vector,
}

#[repr(u8)]
pub enum AllocType {
    Const,
    Heap16,
    Heap32,
    Stack,
}

pub struct Header {
    pub type_tag: TypeTag,
}

pub struct Float {
    pub header: Header,
}

pub struct Int {
    pub header: Header,
}
