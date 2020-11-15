//! Collisions in space.

/// A trait for spatial objects which can collide.
pub trait Collides<Other = Self> {
    /// Returns whether `self` and `other` occupy the same space.
    fn collides(&self, other: &Other) -> bool;
}
