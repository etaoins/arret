use std::sync::atomic::{fence, AtomicU64, Ordering};
use std::{alloc, ptr};

/// Reference count used for global constants created by codegen
const GLOBAL_CONSTANT_REFCOUNT: u64 = std::u64::MAX;

/// Header for shared string data
///
/// This is only separated out to make it easier to calculate allocation sizes.
#[repr(C)]
struct DataHeader {
    ref_count: AtomicU64,
    len: u64,
}

/// Internal shared string data
#[repr(C)]
struct SharedStrData {
    header: DataHeader,
    // This is actually variable length
    data: [u8; 1],
}

impl SharedStrData {
    #[allow(clippy::cast_ptr_alignment)]
    fn new(value: &str) -> *mut SharedStrData {
        unsafe {
            let layout = Self::layout_for_byte_len(value.len());
            let shared_str = alloc::alloc(layout) as *mut SharedStrData;

            (*shared_str).header = DataHeader {
                ref_count: AtomicU64::new(1),
                len: value.len() as u64,
            };

            ptr::copy(
                value.as_ptr(),
                &mut (*shared_str).data[0] as *mut _,
                value.len(),
            );

            shared_str
        }
    }

    fn as_str(&self) -> &str {
        unsafe {
            let utf8 = std::slice::from_raw_parts(&self.data[0], self.header.len as usize);
            std::str::from_utf8_unchecked(utf8)
        }
    }

    fn layout_for_byte_len(len: usize) -> alloc::Layout {
        alloc::Layout::new::<DataHeader>()
            // We have a static length of 1 so we need to allocate at least that
            .extend(alloc::Layout::array::<u8>(std::cmp::max(1, len)).unwrap())
            .unwrap()
            .0
            .pad_to_align()
    }

    fn is_global_constant(&self) -> bool {
        // This doesn't need any ordering; constants can't become non-constant and vice-versa
        self.header.ref_count.load(Ordering::Relaxed) == GLOBAL_CONSTANT_REFCOUNT
    }

    /// Atomically takes a new reference to the string data
    fn take_ref(&mut self) -> *mut SharedStrData {
        if self.is_global_constant() {
            return self;
        }

        // In the case of refing to pass to another thread is sufficient to make the refcount
        // increment itself visible. In the case of one thread incrementing and then decrementing
        // later the decrement itself will enforce memory ordering. This ensures other threads
        // won't falsely destroy the instance.
        self.header.ref_count.fetch_add(1, Ordering::Relaxed);
        self
    }

    /// Atomically releases a reference to the string data
    ///
    /// It's unsafe to use the string data after calling this function.
    unsafe fn release_ref(&mut self) {
        if self.is_global_constant() {
            return;
        }

        let should_destroy = self.header.ref_count.fetch_sub(1, Ordering::Release) == 1;

        if should_destroy {
            // Make sure the memory operations from this delete are strictly after the fetch_sub
            fence(Ordering::Acquire);

            alloc::dealloc(
                self as *mut Self as *mut u8,
                Self::layout_for_byte_len(self.header.len as usize),
            );
        }
    }
}

/// Smart pointer for string data
///
/// This is morally equivalent to `Arc<str>` except it has a fixed FFI representation. This allows
/// codegen to create `SharedStr` instances.
#[repr(transparent)]
pub(super) struct SharedStr {
    data: *mut SharedStrData,
}

impl SharedStr {
    pub(super) fn new(value: &str) -> SharedStr {
        SharedStr {
            data: SharedStrData::new(value),
        }
    }

    /// Returns the shared string slice
    pub(super) fn as_str(&self) -> &str {
        unsafe { (*self.data).as_str() }
    }
}

impl From<&str> for SharedStr {
    fn from(value: &str) -> SharedStr {
        SharedStr::new(value)
    }
}

impl Clone for SharedStr {
    fn clone(&self) -> SharedStr {
        unsafe {
            SharedStr {
                data: (*self.data).take_ref(),
            }
        }
    }
}

impl Drop for SharedStr {
    fn drop(&mut self) {
        unsafe {
            (*self.data).release_ref();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn construction() {
        let empty = SharedStr::new("");
        assert_eq!("", empty.as_str());

        let hello_world = SharedStr::new("Hello, world!");
        assert_eq!("Hello, world!", hello_world.as_str());
    }

    #[test]
    fn cloning() {
        #[allow(clippy::redundant_clone)]
        let hello_clone = SharedStr::new("Hello, clone!").clone();
        assert_eq!("Hello, clone!", hello_clone.as_str());
    }
}
