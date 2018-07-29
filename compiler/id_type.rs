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
    ($counter_name:ident, $id_name:ident, $ty:ty) => {
        #[derive(Clone)]
        pub struct $counter_name($ty);

        impl $counter_name {
            pub fn new() -> $counter_name {
                $counter_name(0)
            }

            pub fn alloc(&mut self) -> $id_name {
                let id = $id_name(self.0);
                self.0 += 1;
                id
            }
        }

        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
        pub struct $id_name($ty);

        impl $id_name {
            #[allow(unused)]
            pub fn new(value: $ty) -> $id_name {
                $id_name(value)
            }
        }
    };
}
