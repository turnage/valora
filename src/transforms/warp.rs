use poly::{Point, Poly};
use rand::Rng;
use rand::distributions::{IndependentSample, Normal};
use num::Signed;
use std::rc::Rc;

/// Specifies which vertices should be warped.
#[derive(Clone, Copy, Debug)]
pub enum WarpCoverage {
    OddVertices,
    AllVertices,
}

/// Specifies in which direction warped vertices should be able to move in
/// relation to the centroid.
#[derive(Clone, Copy, Debug)]
pub enum WarpExpansion {
    Inward,
    Outward,
    Neutral,
}

#[derive(Clone)]
pub struct WarpCfg {
    /// Whether the warp of a given vertex should have a strength proportional
    /// to the distance of its neighboring points (i.e. it can be warped
    /// farther if its neighbors are farther apart).
    pub adapt_to_neighbors: bool,
    /// Variance of the normal distribution used to warp the vertices.
    pub variance: f32,
    /// Which vertices to warp. Warping all vertices results in more uniform
    /// shapes if the vertices adapt to neighbors.
    pub coverage: WarpCoverage,
    /// Whether all warp operations should share the same random number
    /// samples. This yields symmetrical warps.
    pub share_sample: bool,
    /// Optional function to use to adjust the strength of the warp function
    /// at a given point (the strength is multiplied by the result).
    pub spatial_adapter: Option<Rc<Fn(Point) -> f32>>,
    /// Which direction the warped vertices may move relative to centroid.
    pub expansion: WarpExpansion,
    /// Strength factors to use on vertices; expanded to the whole vertex list
    /// so if there are two elements, the first applies to the first half of
    /// warped vertices, the next to the second half, and so on. When there are
    /// more factors than vertices some will not be considered.
    pub custom_factors: Vec<f32>,
}

impl Default for WarpCfg {
    fn default() -> Self {
        Self {
            adapt_to_neighbors: true,
            variance: 0.0,
            coverage: WarpCoverage::AllVertices,
            share_sample: false,
            spatial_adapter: None,
            expansion: WarpExpansion::Outward,
            custom_factors: Vec::new(),
        }
    }
}

pub fn warp<R: Rng>(poly: Poly, cfg: &WarpCfg, rng: &mut R) -> Poly {
    let dist = Normal::new(0.0, cfg.variance.sqrt() as f64);
    let static_sample: Point = dist.ind_sample(rng);
    let center = poly.center();
    let vertices = poly.vertices();
    let n = vertices.len();
    Poly::Irregular(
        vertices
            .clone()
            .into_iter()
            .enumerate()
            .map(move |(i, v)| match (i % 2 == 0, cfg.coverage) {
                (_, WarpCoverage::AllVertices) | (false, WarpCoverage::OddVertices) => {
                    let neighbor_factor = if cfg.adapt_to_neighbors {
                        let left = if i == 0 {
                            vertices[vertices.len() - 1]
                        } else {
                            vertices[i - 1]
                        };
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
                    let delta = if cfg.share_sample {
                        static_sample
                    } else {
                        dist.ind_sample(rng)
                    };
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
    )
}
