use std::ops::Deref;
use std::ptr;
use std::{fmt, hash};

use crate::boxed::Boxed;

/// Reference to a garbage collected value
///
/// This is not memory safe and does not GC root; it's just sugar for a raw pointer.
#[repr(transparent)]
pub struct Gc<T: Boxed> {
    inner: ptr::NonNull<T>,
}

/// Manual Clone implementation to work around Rust issue #26925
impl<T: Boxed> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { inner: self.inner }
    }
}

impl<T: Boxed> Copy for Gc<T> {}

impl<T: Boxed> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.inner.as_ref() }
    }
}

impl<T: Boxed> Gc<T> {
    pub unsafe fn new(ptr: *const T) -> Gc<T> {
        Gc {
            inner: ptr::NonNull::new_unchecked(ptr as *mut T),
        }
    }

    pub unsafe fn cast<U: Boxed>(self) -> Gc<U> {
        Gc {
            inner: self.inner.cast::<U>(),
        }
    }

    pub fn as_ptr(self) -> *const T {
        self.inner.as_ptr()
    }

    pub(super) fn as_mut_ptr(self) -> *mut T {
        self.inner.as_ptr()
    }
}

impl<T> PartialEq for Gc<T>
where
    T: Boxed + PartialEq,
{
    fn eq(&self, other: &Gc<T>) -> bool {
        unsafe { *self.as_ptr() == *other.as_ptr() }
    }
}

impl<T> Eq for Gc<T> where T: Boxed + Eq {}

impl<T> hash::Hash for Gc<T>
where
    T: Boxed + hash::Hash,
{
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        unsafe { (*self.as_ptr()).hash(hasher) }
    }
}

impl<T: Boxed> fmt::Debug for Gc<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        unsafe { (*self.as_ptr()).fmt(formatter) }
    }
}
