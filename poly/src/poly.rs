//! Simple polygon trait for polygons consisting of a single contour.

use point::Point;
use properties::{Centered, Distance};
use std::f32;
use transforms::*;
use rand::Rng;
use rand::distributions::{IndependentSample, Normal};
use num::Signed;

pub trait Poly: Sized {
    fn vertices(&self) -> Vec<Point>;
    fn as_irregular(&self) -> IrregularPoly { IrregularPoly { vertices: self.vertices() } }
}

impl<P: Poly> Centered for P {
    fn center(&self) -> Point { self.vertices().center() }
}

#[derive(Debug, Clone)]
pub struct IrregularPoly {
    pub vertices: Vec<Point>,
}

impl SubdivideEdges for IrregularPoly {
    fn subdivide_edges(self) -> Self {
        Self {
            vertices: self.vertices
                .iter()
                .zip(self.vertices
                         .iter()
                         .skip(1)
                         .chain(self.vertices.iter().take(1)))
                .flat_map(|(p1, p2)| vec![*p1, p1.midpoint(p2)])
                .collect(),
        }
    }
}

impl Warp for IrregularPoly {
    fn warp<R: Rng>(self, cfg: WarpCfg, rng: &mut R) -> Self {
        let dist = Normal::new(0.0, cfg.variance.sqrt() as f64);
        let static_sample: Point = dist.ind_sample(rng);
        let center = self.center();
        let vertices = self.vertices();
        let n = vertices.len();
        IrregularPoly {
            vertices: self.vertices
                .into_iter()
                .enumerate()
                .map(move |(i, v)| match (i % 2 == 0, cfg.coverage) {
                         (_, WarpCoverage::AllVertices) |
                         (false, WarpCoverage::OddVertices) => {
                    let neighbor_factor = if cfg.adapt_to_neighbors {
                        let left =
                            if i == 0 { vertices[vertices.len() - 1] } else { vertices[i - 1] };
                        let right = vertices[(i + 1) % vertices.len()];
                        left.distance(&right) * 0.5
                    } else {
                        1.0
                    };
                    let custom_factor = if cfg.custom_factors.is_empty() {
                        1.0
                    } else {
                        cfg.custom_factors[i / (n / cfg.custom_factors.len())]
                    };
                    let spatial_factor = match cfg.spatial_adapter {
                        Some(ref f) => f(v),
                        None => 1.0,
                    };
                    let delta = if cfg.share_sample { static_sample } else { dist.ind_sample(rng) };
                    let delta = delta * neighbor_factor * spatial_factor * custom_factor;
                    let out_sign = center.sign_to(&v);
                    let delta = match cfg.expansion {
                        WarpExpansion::Inward => delta.abs() * -out_sign,
                        WarpExpansion::Outward => delta.abs() * out_sign,
                        WarpExpansion::Neutral => delta,
                    };
                    v + delta
                }
                         _ => v,
                     })
                .collect(),
        }
    }
}

impl Poly for IrregularPoly {
    fn vertices(&self) -> Vec<Point> { self.vertices.clone() }
}

impl Translate for IrregularPoly {
    fn translate(self, delta: Point) -> Self {
        IrregularPoly { vertices: self.vertices.into_iter().map(|p| p + delta).collect() }
    }
}

impl Scale for IrregularPoly {
    fn scale(self, scale: f32) -> Self { Self { vertices: self.vertices().scale(scale) } }
}

#[derive(Copy, Clone, Debug)]
pub struct Ngon {
    pub n: usize,
    pub rotation: f32,
    pub radius: f32,
    pub center: Point,
}

impl Poly for Ngon {
    fn vertices(&self) -> Vec<Point> {
        use ellipse::Ellipse;
        Ellipse::circle(self.center, self.radius).circumpoints(self.n, self.rotation)
    }
}
