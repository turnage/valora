//! Quadratic Bezier segments.

use super::{Curve, Intersection};
use crate::{bounds::Bounds, V2};
use roots::{find_roots_quadratic, Roots};
use std::ops::Range;
use vek::{bezier::repr_c::QuadraticBezier2, vec::repr_c::vec2::Vec2};

const EXCLUSIVE_DOMAIN: Range<f32> = (0. + std::f32::EPSILON)..1.0;

#[derive(Debug)]
pub enum CreateQuadraticResult {
    Quadratic(Vec<QuadraticBezier>),
    IsLineSegment { start: V2, end: V2 },
}

/// A monotonic (with respect to x and y) quadratic bezier curve.
#[derive(Debug)]
pub struct QuadraticBezier {
    inner: QuadraticBezier2<f32>,
    bounds: Bounds,
}

impl From<QuadraticBezier2<f32>> for QuadraticBezier {
    fn from(inner: QuadraticBezier2<f32>) -> Self {
        let (left, right) = inner.x_bounds();
        let (bottom, top) = inner.y_bounds();
        Self {
            bounds: Bounds {
                left,
                right,
                top,
                bottom,
            },
            inner,
        }
    }
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
                CreateQuadraticResult::Quadratic(
                    [c0, c1, c2].iter().copied().map(Self::from).collect(),
                )
            }
            (Some(t), None) | (None, Some(t)) => {
                let (c0, c1) = curve.split(t);
                CreateQuadraticResult::Quadratic([c0, c1].iter().copied().map(Self::from).collect())
            }
            _ => CreateQuadraticResult::IsLineSegment { start, end },
        }
    }
}

impl Curve for QuadraticBezier {
    fn sample_t(&self, t: f32) -> Option<V2> {
        if t < 0.0 || t > 1.0 {
            return None;
        }

        let p = self.inner.evaluate(t);
        Some(V2::new(p.x, p.y))
    }

    fn sample_y(&self, y: f32) -> Option<Intersection> {
        let (c0, c1, c2) = self.inner.into_tuple();
        let (c0, c1, c2) = (c0.y, c1.y, c2.y);
        let coef_pow_2 = c0 - 2. * c1 + c2;
        let coef_pow_1 = 2. * c1 - 2. * c0;
        let coef_pow_0 = c0 - y;
        match find_roots_quadratic(coef_pow_2, coef_pow_1, coef_pow_0) {
            Roots::One([t]) | Roots::Two([t, _]) => {
                Some(t.abs()).filter(|t| EXCLUSIVE_DOMAIN.contains(t))
            }
            _ => None,
        }
        .map(|t| Intersection {
            axis: self.inner.evaluate(t).x,
            t,
        })
    }

    fn sample_x(&self, x: f32) -> Option<Intersection> {
        let (c0, c1, c2) = self.inner.into_tuple();
        let (c0, c1, c2) = (c0.x, c1.x, c2.x);
        let coef_pow_2 = c0 - 2. * c1 + c2;
        let coef_pow_1 = 2. * c1 - 2. * c0;
        let coef_pow_0 = c0 - x;
        match find_roots_quadratic(coef_pow_2, coef_pow_1, coef_pow_0) {
            Roots::One([t]) | Roots::Two([t, _]) => {
                Some(t.abs()).filter(|t| EXCLUSIVE_DOMAIN.contains(t))
            }
            _ => None,
        }
        .map(|t| Intersection {
            axis: self.inner.evaluate(t).y,
            t,
        })
    }

    fn bounds(&self) -> &Bounds { &self.bounds }
}
