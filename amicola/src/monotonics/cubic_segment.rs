//! Cubic Bezier segments.

use super::{Curve, Intersection};
use crate::{bounds::Bounds, V2};
use float_ord::FloatOrd;
use log::trace;
use roots::{find_roots_cubic, Roots};
use std::ops::Range;
use vek::{bezier::repr_c::CubicBezier2, vec::repr_c::vec2::Vec2};

const EXCLUSIVE_DOMAIN: Range<f32> = (0. + std::f32::EPSILON)..1.0;

#[derive(Debug)]
pub enum CreateCubicResult {
    Cubic(Vec<CubicBezier>),
    IsLineSegment { start: V2, end: V2 },
}

/// A monotonic (with respect to x and y) Cubic bezier curve.
#[derive(Debug)]
pub struct CubicBezier {
    inner: CubicBezier2<f32>,
    bounds: Bounds,
}

impl From<CubicBezier2<f32>> for CubicBezier {
    fn from(inner: CubicBezier2<f32>) -> Self {
        let (left, right) = inner.x_bounds();
        let (left, right) = (inner.evaluate(left).x, inner.evaluate(right).x);

        let (bottom, top) = inner.y_bounds();
        let (bottom, top) = (inner.evaluate(bottom).y, inner.evaluate(top).y);
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

impl CubicBezier {
    pub fn new(start: V2, ctrl0: V2, ctrl1: V2, end: V2) -> CreateCubicResult {
        let curve = CubicBezier2 {
            start: Vec2::new(start.x, start.y),
            ctrl0: Vec2::new(ctrl0.x, ctrl0.y),
            ctrl1: Vec2::new(ctrl1.x, ctrl1.y),
            end: Vec2::new(end.x, end.y),
        };

        let inflections = {
            let mut inflections = curve_inflections(&curve);
            inflections.sort_by_key(|t| FloatOrd(t * -1.));
            inflections
        };
        if inflections.is_empty() {
            return CreateCubicResult::IsLineSegment { start, end };
        }

        let (mut curves, last_curve, _) = inflections.into_iter().fold(
            (vec![], curve, 1.),
            |(mut curves, current_curve, prop), t| {
                let (left, right) = current_curve.split(t / prop);
                curves.push(right);
                (curves, left, t)
            },
        );
        curves.push(last_curve);
        CreateCubicResult::Cubic(curves.into_iter().map(Self::from).collect())
    }
}

fn curve_inflections(curve: &CubicBezier2<f32>) -> Vec<f32> {
    let mut inflections = vec![];
    if let Some((rx1, maybe_rx2)) = curve.x_inflections() {
        if let Some(rx2) = maybe_rx2 {
            inflections.push(rx2);
        }

        inflections.push(rx1);
    }

    if let Some((ry1, maybe_ry2)) = curve.y_inflections() {
        if let Some(ry2) = maybe_ry2 {
            inflections.push(ry2);
        }
        inflections.push(ry1);
    }

    inflections
}

impl Curve for CubicBezier {
    fn sample_t(&self, t: f32) -> Option<V2> {
        if t < 0.0 || t > 1.0 {
            return None;
        }

        let p = self.inner.evaluate(t);
        Some(V2::new(p.x, p.y))
    }

    fn sample_y(&self, y: f32) -> Option<Intersection> {
        let (c0, c1, c2, c3) = self.inner.into_tuple();
        let (c0, c1, c2, c3) = (c0.y, c1.y, c2.y, c3.y);
        let coef_pow_3 = 3. * c1 - c0 - 3. * c2 + c3;
        let coef_pow_2 = 3. * c0 - 6. * c1 + 3. * c2;
        let coef_pow_1 = 3. * c1 - 3. * c0;
        let coef_pow_0 = c0 - y;

        trace!(
            "(3 * {:?}) - {:?} + (3 * {:?}) + {:?} = {:?}",
            c1,
            c0,
            c2,
            c3,
            coef_pow_3
        );
        trace!("control points: {:?}", self.inner.into_tuple());
        trace!(
            "Coefs: {:?}, {:?}, {:?}, {:?}",
            coef_pow_3,
            coef_pow_2,
            coef_pow_1,
            coef_pow_0
        );
        let roots = find_roots_cubic(coef_pow_3, coef_pow_2, coef_pow_1, coef_pow_0);
        let result = roots
            .as_ref()
            .iter()
            .copied()
            .filter(|t| EXCLUSIVE_DOMAIN.contains(&t))
            .next()
            .map(|t| Intersection {
                axis: self.inner.evaluate(t).x,
                t,
            });
        trace!(
            "For cubic segment, found intersection for y {:?}: {:#?}",
            y,
            roots
        );
        result
    }

    fn sample_x(&self, x: f32) -> Option<Intersection> {
        let (c0, c1, c2, c3) = self.inner.into_tuple();
        let (c0, c1, c2, c3) = (c0.x, c1.x, c2.x, c3.x);
        let coef_pow_3 = 3. * c1 - c0 - 3. * c2 + c3;
        let coef_pow_2 = 3. * c0 - 6. * c1 + 3. * c2;
        let coef_pow_1 = 3. * c1 - 3. * c0;
        let coef_pow_0 = c0 - x;

        find_roots_cubic(coef_pow_3, coef_pow_2, coef_pow_1, coef_pow_0)
            .as_ref()
            .iter()
            .copied()
            .map(f32::abs)
            .filter(|t| EXCLUSIVE_DOMAIN.contains(&t))
            .next()
            .map(|t| Intersection {
                axis: self.inner.evaluate(t).y,
                t,
            })
    }

    fn bounds(&self) -> &Bounds { &self.bounds }

    fn bookends(&self) -> (V2, V2) {
        let (start, _, _, end) = self.inner.into_tuple();
        (V2::new(start.x, start.y), V2::new(end.x, end.y))
    }
}
