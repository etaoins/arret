//! References to boxed values
//!
//! These are all transparent; they're used for either readability or marker traits.

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

// Manual Clone implementation to work around Rust issue #26925
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
    /// Returns a new instance wrapping a pointer to a garbage collected box
    pub unsafe fn new(ptr: *const T) -> Gc<T> {
        Gc {
            inner: ptr::NonNull::new_unchecked(ptr as *mut T),
        }
    }

    /// Unchecked cast to the passed type
    pub unsafe fn cast<U: Boxed>(self) -> Gc<U> {
        Gc {
            inner: self.inner.cast::<U>(),
        }
    }

    /// Returns a pointer to the garbage collected box
    pub fn as_ptr(self) -> *const T {
        self.inner.as_ptr()
    }

    /// Returns a mutable to the garbage collected box
    pub(super) fn as_mut_ptr(self) -> *mut T {
        self.inner.as_ptr()
    }
}

impl<T: Boxed> PartialEq for Gc<T> {
    fn eq(&self, other: &Gc<T>) -> bool {
        unsafe { *self.as_ptr() == *other.as_ptr() }
    }
}

impl<T> Eq for Gc<T> where T: Boxed + Eq {}

impl<T: Boxed> hash::Hash for Gc<T> {
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

macro_rules! define_marker_ref {
    (
        $(#[$docs:meta])*
        $ref_name:ident
    ) => {
        $(#[$docs])*
        #[repr(transparent)]
        pub struct $ref_name<T: Boxed> {
            inner: ptr::NonNull<T>,
        }

        impl<T: Boxed> Deref for $ref_name<T> {
            type Target = T;
            fn deref(&self) -> &T {
                unsafe { self.inner.as_ref() }
            }
        }

        impl<T: Boxed> From<$ref_name<T>> for Gc<T> {
            fn from(marker_ref: $ref_name<T>) -> Gc<T> {
                Gc {
                    inner: marker_ref.inner,
                }
            }
        }
    };
}

define_marker_ref!(
    /// Special marker ref for parameters that are explicitly not captured
    ///
    /// This can be used for performance-sensitive functions where the compiler cannot prove the
    /// parameter can't be captured.
    NoCapture
);

define_marker_ref!(
    /// Special marker ref for parameters that are explicitly captured
    ///
    /// Capturing GC managed values is usually not allowed as the captured values become invisible
    /// to the garbage collector and will become invalid on the next collection cycle. This is
    /// intended for use by special runtime functions that expose their captured values to the
    /// collector via an internal mechanism.
    Capture
);
