use crate::P2;

/// A trait for forms which can enclose a point.
pub trait Contains {
    /// Returns true if `p` is within the boundary of `self`.
    fn contains(&self, p: P2) -> bool;
}
