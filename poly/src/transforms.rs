//! Transformations on polygons.

use point::Point;
use properties::Centered;
use rand::Rng;
use std::rc::Rc;

/// Percent of a thing; usually used for paths but may be implemented with
/// other semantics for different types.
pub trait Percent: Sized {
    fn percent(self, percent: f32) -> Self;
}

/// Place a type at the given point. Equivalent to calculating the delta to a point and translating
/// that delta.
pub trait Place: Sized {
    fn place(self, dest: Point) -> Self;
}

impl<T: Translate + Centered> Place for T {
    fn place(self, dest: Point) -> Self {
        let delta = dest - self.center();
        self.translate(delta)
    }
}

pub trait Translate: Sized {
    fn translate(self, delta: Point) -> Self;
}

pub trait Scale {
    fn scale(self, scale: f32) -> Self;
}

// Cuts the edges of the geometry in half so that points exist at the
// midpoint of all previously existing edges. The actual shape should
// not change.
pub trait SubdivideEdges: Sized {
    fn subdivide_edges(self) -> Self;
}

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

pub trait Warp: Sized + Centered {
    fn warp<R: Rng>(self, cfg: WarpCfg, rng: &mut R) -> Self;
}
