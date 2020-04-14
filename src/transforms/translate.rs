//! Translations.

use crate::{P2, V2};

/// A trait for spatially translatable types.
pub trait Translate {
    /// Translate `self` by the given translation vector.
    fn translate(self, translation: V2) -> Self;
}

impl Translate for P2 {
    fn translate(self, translation: V2) -> Self {
        self + translation
    }
}
