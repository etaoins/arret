use boxed::Header;

#[repr(C, align(16))]
pub struct Nil {
    pub header: Header,
}
