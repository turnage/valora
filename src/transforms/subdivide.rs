//! Subdivisions.

/// A trait for types which can subdivide and retain characteristics.
pub trait Subdivide {
    /// Subdivides by one order while retaining shape.
    fn subdivide(self) -> Self;
}
