use std::hash::Hash;

/// Mark to indicate an item can be dispatched to registered code.
pub trait Dispatchable: Hash + Eq + Clone {}
