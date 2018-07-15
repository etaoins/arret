use boxed::Header;

#[repr(C)]
pub struct Nil {
    pub header: Header,
}
