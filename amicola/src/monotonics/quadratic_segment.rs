//! Quadratic Bezier segments.

use super::Curve;
use crate::V2;

use vek::{bezier::repr_simd::QuadraticBezier2, vec::repr_simd::vec2::Vec2};

#[derive(Debug)]
pub enum CreateQuadraticResult {
    Quadratic(Vec<QuadraticBezier>),
    IsLineSegment { start: V2, end: V2 },
}

/// A monotonic quadratic bezier curve.
#[derive(Debug)]
pub struct QuadraticBezier {
    inner: QuadraticBezier2<f32>,
}

impl QuadraticBezier {
    pub fn new(start: V2, ctrl: V2, end: V2) -> CreateQuadraticResult {
        let curve = QuadraticBezier2 {
            start: Vec2::new(start.x, start.y),
            ctrl: Vec2::new(ctrl.x, ctrl.y),
            end: Vec2::new(end.x, end.y),
        };

        match (curve.y_inflection(), curve.x_inflection()) {
            (Some(yt), Some(xt)) => {
                let (c0, c1) = curve.split(yt);
                let (c0, c1, c2) = if yt > xt {
                    let (c1, c2) = c1.split((yt - xt) / (1. - xt));
                    (c0, c1, c2)
                } else {
                    let c2 = c1;
                    let (c0, c1) = c0.split(yt / xt);
                    (c0, c1, c2)
                };
                CreateQuadraticResult::Quadratic(vec![
                    Self { inner: c0 },
                    Self { inner: c1 },
                    Self { inner: c2 },
                ])
            }
            (Some(t), None) | (None, Some(t)) => {
                let (c0, c1) = curve.split(t);
                CreateQuadraticResult::Quadratic(vec![Self { inner: c0 }, Self { inner: c1 }])
            }
            _ => CreateQuadraticResult::IsLineSegment { start, end },
        }
    }
}
