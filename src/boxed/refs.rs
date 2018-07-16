use std::ops::Deref;
use std::ptr;

/// Reference to a garbage collected value
///
/// This is not memory safe and does not GC root; it's just sugar for a raw pointer.
pub struct Gc<T> {
    pub inner: ptr::NonNull<T>,
}

/// Manual Clone implementation to work around Rust issue #26925
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { inner: self.inner }
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.inner.as_ref() }
    }
}

impl<T> Gc<T> {
    pub unsafe fn new(ptr: *const T) -> Gc<T> {
        Gc {
            inner: ptr::NonNull::new_unchecked(ptr as *mut T),
        }
    }
}
