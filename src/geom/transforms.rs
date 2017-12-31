use color::Opacity;
use geom::{Centered, Distance, IrregularPoly, Point, Poly};
use mesh::Mesh;
use rand::Rng;
use rand::distributions::{IndependentSample, Normal};
use std::rc::Rc;

pub trait SubdivideEdges: Sized {
    // Cuts the edges of the geometry in half so that points exist at the
    // midpoint of all previously existing edges. The actual shape should
    // not change.
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

impl Warp for IrregularPoly {
    fn warp<R: Rng>(self, cfg: WarpCfg, rng: &mut R) -> Self {
        let dist = Normal::new(0.0, cfg.variance.sqrt() as f64);
        let static_sample: Point = dist.ind_sample(rng);
        let centroid = self.centroid();
        let vertices = self.vertices();
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
                    let spatial_factor = match cfg.spatial_adapter {
                        Some(ref f) => f(v),
                        None => 1.0,
                    };
                    let delta = if cfg.share_sample { static_sample } else { dist.ind_sample(rng) };
                    let delta = delta * neighbor_factor * spatial_factor;
                    let out_sign = centroid.sign_to(&v);
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

pub struct WaterColorCfg {
    pub layers: usize,
    pub spread: f32,
    pub depth: usize,
    pub opacity: f32,
}

pub trait WaterColor: Clone {
    fn watercolor<R: Rng, F: Fn(Self, usize) -> Mesh<Self>>(self,
                                                            cfg: WaterColorCfg,
                                                            f: F,
                                                            rng: &mut R)
                                                            -> Vec<Mesh<Self>>;
}

impl<W: SubdivideEdges + Warp + Clone> WaterColor for W {
    fn watercolor<R: Rng, F: Fn(Self, usize) -> Mesh<Self>>(self,
                                                            cfg: WaterColorCfg,
                                                            f: F,
                                                            rng: &mut R)
                                                            -> Vec<Mesh<Self>> {
        use transforms::iterate_rand;

        (0..(cfg.layers))
            .into_iter()
            .map(|i| {
                let percent = (i as f32) / (cfg.layers as f32);
                let warp_cfg = WarpCfg {
                    variance: cfg.spread + cfg.spread * percent,
                    coverage: WarpCoverage::AllVertices,
                    ..WarpCfg::default()
                };
                f(iterate_rand(self.clone(), cfg.depth, rng, |splotch, rng| {
                    splotch.subdivide_edges().warp(warp_cfg.clone(), rng)
                }),
                  i)
                        .opacity(cfg.opacity)
            })
            .collect()
    }
}