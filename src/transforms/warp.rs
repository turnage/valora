use geom::Point;
use properties::Centered;
use rand::Rng;
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
}

impl Default for WarpCfg {
    fn default() -> Self {
        Self {
            adapt_to_neighbors: true,
            variance: 0.0,
            coverage: WarpCoverage::OddVertices,
            share_sample: false,
            spatial_adapter: None,
            expansion: WarpExpansion::Outward,
        }
    }
}

pub trait Warp: Sized + Centered {
    fn warp<R: Rng>(self, cfg: WarpCfg, rng: &mut R) -> Self;
}