//! Scale

/// A trait for types which can change their scale.
pub trait Scale {
    /// Scales the type by the given factor against a default anchor point.
    fn scale(self, factor: f32) -> Self;
}
