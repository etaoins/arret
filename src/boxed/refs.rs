use std::ops::Deref;
use std::ptr;

use boxed::Boxed;

/// Reference to a garbage collected value
///
/// This is not memory safe and does not GC root; it's just sugar for a raw pointer.
pub struct Gc<T>
where
    T: Boxed,
{
    inner: ptr::NonNull<T>,
}

/// Manual Clone implementation to work around Rust issue #26925
impl<T> Clone for Gc<T>
where
    T: Boxed,
{
    fn clone(&self) -> Self {
        Gc { inner: self.inner }
    }
}

impl<T> Copy for Gc<T> where T: Boxed {}

impl<T> Deref for Gc<T>
where
    T: Boxed,
{
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.inner.as_ref() }
    }
}

impl<T> Gc<T>
where
    T: Boxed,
{
    pub unsafe fn new(ptr: *const T) -> Gc<T> {
        Gc {
            inner: ptr::NonNull::new_unchecked(ptr as *mut T),
        }
    }

    pub unsafe fn cast<U>(self) -> Gc<U>
    where
        U: Boxed,
    {
        Gc {
            inner: self.inner.cast::<U>(),
        }
    }

    pub fn as_ptr(self) -> *const T {
        self.inner.as_ptr()
    }
}
