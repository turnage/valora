//! Subdivisions.

/// A trait for types which can subdivide and retain characteristics.
pub trait Subdivide: Sized {
    /// Subdivides by one order while retaining shape.
    fn subdivide(self) -> Self;

    // Subdivides by n orders while retaining shape.
    fn subdivide_n(self, n: usize) -> Self {
        let mut result = self;
        for _ in 0..n {
            result = result.subdivide();
        }
        result
    }
}
