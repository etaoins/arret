/// Builds a static NULL terminated `*const libc::c_char` with the given contents
#[macro_export]
macro_rules! libcstr {
    ($s:expr) => {
        concat!($s, "\0").as_ptr() as *const libc::c_char
    };
}
