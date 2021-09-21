//!  This module adds shims for BTreeSet methods that not yet stable.

use std::collections::BTreeSet;

/// This trait adds shims for BTreeSet methods that not yet stable. They have
/// the same behavior as their nightly counterparts, but additionally require
/// `Clone` bound on the element type (for `pop_first` and `pop_last`).
pub trait BTreeSetShims<T> {
    /// Returns a reference to the first value in the set, if any. This value is
    /// always the minimum of all values in the set.
    fn first_shim(&self) -> Option<&T>;

    /// Returns a reference to the last value in the set, if any. This value is
    /// always the maximum of all values in the set.
    fn last_shim(&self) -> Option<&T>;

    /// Removes the first value from the set and returns it, if any. The first
    /// value is always the minimum value in the set.
    fn pop_first_shim(&mut self) -> Option<T>;

    /// Removes the last value from the set and returns it, if any. The last
    /// value is always the maximum value in the set.
    fn pop_last_shim(&mut self) -> Option<T>;
}

impl<T: Ord + Clone> BTreeSetShims<T> for BTreeSet<T> {
    fn first_shim(&self) -> Option<&T> {
        let mut iter = self.iter();
        iter.next()
    }

    fn last_shim(&self) -> Option<&T> {
        let iter = self.iter();
        iter.last()
    }

    fn pop_first_shim(&mut self) -> Option<T> {
        let mut iter = self.iter();
        let first = iter.next().cloned();
        if let Some(first) = first {
            return self.take(&first);
        }
        None
    }

    fn pop_last_shim(&mut self) -> Option<T> {
        let iter = self.iter();
        let last = iter.last().cloned();
        if let Some(last) = last {
            return self.take(&last);
        }
        None
    }
}
