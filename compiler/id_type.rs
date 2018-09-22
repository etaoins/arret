/// Builds a new ID type based off indexing in to a Vec lookup table
///
/// This stores the value internally at $ty (typically `u32`) while the interface uses `usize` to
/// support easy indexing.
#[macro_export]
macro_rules! new_indexing_id_type {
    ($name:ident, $ty:ty) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
        pub struct $name($ty);

        impl $name {
            pub fn new(value: usize) -> $name {
                $name(value as $ty)
            }

            #[allow(unused)]
            pub fn new_entry_id<T>(lookup_vec: &mut Vec<T>, entry: T) -> $name {
                let id = Self::new(lookup_vec.len());
                lookup_vec.push(entry);
                id
            }

            #[allow(unused)]
            pub fn to_usize(&self) -> usize {
                self.0 as usize
            }
        }
    };
}

/// Builds a new ID type based off an arbitrary counter
///
/// This is used for IDs that are simply allocated sequentially. Their interface type matches their
/// internal type (typically `u32`).
#[macro_export]
macro_rules! new_counting_id_type {
    ($id_name:ident) => {
        use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

        static NEXT_VALUE: AtomicUsize = ATOMIC_USIZE_INIT;

        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
        pub struct $id_name(usize);

        impl $id_name {
            pub fn alloc() -> $id_name {
                $id_name(NEXT_VALUE.fetch_add(1, Ordering::SeqCst) + 1)
            }
        }
    };
}
