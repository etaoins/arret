use std::alloc;

/// Allocation to store a record's data
#[repr(C)]
pub struct RecordData {
    data_ptr: *mut u8,
    compact_layout: u64,
}

impl RecordData {
    /// Constructs an empty record data
    pub fn empty() -> Self {
        Self::alloc(alloc::Layout::new::<[u8; 0]>())
    }

    /// Allocates record data for the given layout
    pub fn alloc(data_layout: alloc::Layout) -> Self {
        unsafe {
            Self {
                data_ptr: alloc::alloc(data_layout),
                compact_layout: Self::alloc_layout_to_u64(data_layout),
            }
        }
    }

    /// Returns a pointer to the record data
    pub fn as_ptr(&self) -> *const u8 {
        self.data_ptr
    }

    /// Returns a mutable pointer to the record data
    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.data_ptr
    }

    /// Returns the layout for the record data
    pub fn layout(&self) -> alloc::Layout {
        Self::u64_to_alloc_layout(self.compact_layout)
    }

    fn alloc_layout_to_u64(alloc_layout: alloc::Layout) -> u64 {
        // This allows for alignments up to 2^16 and sizes up to 2^48
        ((alloc_layout.align() as u64) & 0xFFFF) | ((alloc_layout.size() as u64) << 16)
    }

    fn u64_to_alloc_layout(input: u64) -> alloc::Layout {
        let align = (input & 0xFFFF) as usize;
        let size = (input >> 16) as usize;

        unsafe { alloc::Layout::from_size_align_unchecked(size, align) }
    }
}

impl Drop for RecordData {
    fn drop(&mut self) {
        let data_layout = Self::u64_to_alloc_layout(self.compact_layout);

        unsafe {
            alloc::dealloc(self.data_ptr as *mut u8, data_layout);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_alloc_layout_to_u64() {
        let u8_layout = alloc::Layout::new::<u8>();
        let u32_layout = alloc::Layout::new::<u32>();
        let u64_layout = alloc::Layout::new::<u64>();
        let empty_array_layout = alloc::Layout::new::<[char; 0]>();
        let large_array_layout = alloc::Layout::new::<[f64; 10000]>();

        for layout in &[
            u8_layout,
            u32_layout,
            u64_layout,
            empty_array_layout,
            large_array_layout,
        ] {
            assert_eq!(
                *layout,
                RecordData::u64_to_alloc_layout(RecordData::alloc_layout_to_u64(*layout)),
            )
        }
    }
}
