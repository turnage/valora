use color::{BlendMode, Colorer};
use generators::spawner::{SpawnCfg, SpawnSrc, Spawner};
use geom::{Point, Poly};
use geom::transforms::SubdivideEdges;
use mesh::{DrawMode, Mesh};
use palette::Colora;
use properties::Centered;
use rand::Rng;
use transforms::iterate_rand;
use transforms::warp::*;

pub struct WaterColorCfg {
    pub layers: usize,
    pub spread: f32,
    pub depth: usize,
    pub color: Colora,
    pub draw_mode: DrawMode,
    pub blend_mode: BlendMode,
    pub anchor_layer: bool,
    pub uniform_spread: bool,
    pub subdivides_per: usize,
}

impl Default for WaterColorCfg {
    fn default() -> Self {
        Self {
            layers: 100,
            spread: 0.03,
            depth: 5,
            color: Colora::rgb(1.0, 1.0, 1.0, 0.04),
            draw_mode: DrawMode::Fill,
            blend_mode: BlendMode::Normal,
            anchor_layer: false,
            uniform_spread: false,
            subdivides_per: 1,
        }
    }
}

enum WaterColorSrc<S> {
    /// An unmodified polygon from which to build a distinct layer.
    Base(S),
    /// An anchor layer already warped; just needs some distinction warps.
    Anchor(S),
}

pub struct WaterColor<S> {
    cfg: WaterColorCfg,
    src: WaterColorSrc<S>,
    custom_factors: Option<Vec<f32>>,
    spawn_point: Point,
}

impl<S: Poly + SubdivideEdges + Warp + Centered + Clone> WaterColor<S> {
    pub fn new<R: Rng>(src: S, cfg: WaterColorCfg, rng: &mut R) -> Self {
        let custom_factors: Option<Vec<f32>> = if cfg.uniform_spread {
            None
        } else {
            Some(src.vertices()
                     .into_iter()
                     .map(|_| rng.gen_range(0.5, 1.0))
                     .collect())
        };
        Self {
            spawn_point: src.centroid(),
            src: if cfg.anchor_layer {
                WaterColorSrc::Anchor(WaterColor::warp(src, &cfg, &custom_factors, rng))
            } else {
                WaterColorSrc::Base(src)
            },
            custom_factors,
            cfg,
        }
    }

    fn warp<R: Rng>(src: S,
                    cfg: &WaterColorCfg,
                    custom_factors: &Option<Vec<f32>>,
                    rng: &mut R)
                    -> S {
        use transforms::iterate;

        let mut src = src;
        for _ in 0..(cfg.depth) {
            src = src.warp(WarpCfg {
                               variance: cfg.spread,
                               custom_factors: custom_factors.clone().unwrap_or(Vec::new()),
                               expansion: WarpExpansion::Inward,
                               ..WarpCfg::default()
                           },
                           rng);
            src = iterate(src, cfg.subdivides_per, |src| src.subdivide_edges());
        }
        src
    }
}

impl<S: SubdivideEdges + Warp + Poly + Clone> Spawner<Mesh<S>> for WaterColor<S> {
    fn spawn(&self, cfg: SpawnCfg) -> Mesh<S> {
        use color::Opacity;
        use transforms::iterate;

        let src = match self.src {
            WaterColorSrc::Base(ref src) => {
                WaterColor::warp(src.clone(), &self.cfg, &self.custom_factors, cfg.rng)
            }
            WaterColorSrc::Anchor(ref src) => {
                WaterColor::warp(src.clone(),
                                 &WaterColorCfg {
                                      subdivides_per: 0,
                                      spread: self.cfg.spread * (self.cfg.depth as f32),
                                      ..self.cfg
                                  },
                                 &self.custom_factors,
                                 cfg.rng)
            }
        };
        Mesh {
            src,
            colorer: Colorer::from(self.cfg.color),
            draw_mode: self.cfg.draw_mode,
            blend_mode: self.cfg.blend_mode,
        }
    }
}

impl<S> SpawnSrc for WaterColor<S> {
    fn spawn_points(&self) -> Vec<Point> {
        (0..self.cfg.layers)
            .into_iter()
            .map(|_| self.spawn_point)
            .collect()
    }
}