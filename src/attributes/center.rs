use crate::P2;

/// A trait for objects which have a spatial center.
pub trait Center {
    /// Returns the center of the object.
    fn center(&self) -> P2;
}
