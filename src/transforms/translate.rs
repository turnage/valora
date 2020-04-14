//! Translations.

use crate::{Angle, Ellipse, P2, V2};

/// A trait for spatially translatable types.
pub trait Translate: Sized {
    /// Translate `self` by the given translation vector.
    fn translate(self, translation: V2) -> Self;

    /// Translate `self` along the given direction.
    fn translate_along(self, direction: Angle, distance: f32) -> Self {
        let offset = Ellipse::circle(P2::new(0., 0.), distance)
            .circumpoint(direction)
            .to_vector();
        self.translate(offset)
    }
}

impl Translate for P2 {
    fn translate(self, translation: V2) -> Self {
        self + translation
    }
}
