//! Container for runtime type information for boxed data

use crate::class_map::ClassMap;
use crate::intern::{AsInterner, Interner};

/// Contains associated runtime type information for boxed data
///
/// This is a container for [`Interner`] and [`ClassMap`].
pub struct TypeInfo {
    interner: Interner,
    class_map: ClassMap,
}

impl TypeInfo {
    /// Constructs type information with the given components
    pub fn new(interner: Interner, class_map: ClassMap) -> TypeInfo {
        TypeInfo {
            interner,
            class_map,
        }
    }

    /// Constructs empty type information
    pub fn empty() -> TypeInfo {
        Self::new(Interner::new(), ClassMap::empty())
    }

    /// Returns a clone of this type information suitable for garbage collection
    pub fn clone_for_collect_garbage(&self) -> Self {
        Self {
            interner: self.interner.clone_for_collect_garbage(),
            class_map: self.class_map.clone(),
        }
    }

    /// Returns the symbol interner
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Returns a mutable reference to the symbol interner
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    /// Returns the class map
    pub fn class_map(&self) -> &ClassMap {
        &self.class_map
    }

    /// Returns a mutable reference to the class map
    pub fn class_map_mut(&mut self) -> &mut ClassMap {
        &mut self.class_map
    }
}

impl AsInterner for TypeInfo {
    fn as_interner(&self) -> &Interner {
        self.interner()
    }
}
