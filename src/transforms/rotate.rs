//! Rotation

use crate::{Angle, Ellipse, P2};

pub trait Rotate {
    fn rotate(self, pivot: P2, theta: Angle) -> Self;
}

impl Rotate for P2 {
    fn rotate(self, pivot: P2, theta: Angle) -> Self {
        let radius = (self - pivot).length();
        let current = Ellipse::circle(pivot, radius);
        let current_theta = current.circumphase(&self);

        current.circumpoint(current_theta + theta)
    }
}
