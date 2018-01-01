use color::{BlendMode, Colorer};
use generators::spawner::{SpawnCfg, SpawnSrc, Spawner};
use geom::Point;
use geom::transforms::SubdivideEdges;
use mesh::{DrawMode, Mesh};
use palette::Colora;
use properties::Centered;
use transforms::warp::{Warp, WarpCfg, WarpCoverage};

pub struct WaterColorCfg {
    pub layers: usize,
    pub spread: f32,
    pub depth: usize,
    pub color: Colora,
    pub draw_mode: DrawMode,
    pub blend_mode: BlendMode,
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
        }
    }
}

pub struct WaterColor<S> {
    cfg: WaterColorCfg,
    src: S,
}

impl<S: SubdivideEdges + Warp + Clone> WaterColor<S> {
    pub fn new(src: S, cfg: WaterColorCfg) -> Self { Self { cfg, src } }
}

impl<S: SubdivideEdges + Warp + Clone> Spawner<Mesh<S>> for WaterColor<S> {
    fn spawn(&self, cfg: SpawnCfg) -> Mesh<S> {
        use transforms::iterate_rand;
        let warp_cfg = WarpCfg {
            variance: self.cfg.spread + self.cfg.spread * cfg.percent,
            coverage: WarpCoverage::AllVertices,
            ..WarpCfg::default()
        };
        Mesh {
            src: iterate_rand(self.src.clone(),
                              self.cfg.depth,
                              cfg.rng,
                              |splotch, rng| splotch.subdivide_edges().warp(warp_cfg.clone(), rng)),
            colorer: Colorer::from(self.cfg.color),
            draw_mode: self.cfg.draw_mode,
            blend_mode: self.cfg.blend_mode,
        }
    }
}

impl<S: Centered> SpawnSrc for WaterColor<S> {
    fn spawn_points(&self) -> Vec<Point> {
        (0..self.cfg.layers)
            .into_iter()
            .map(|_| self.src.centroid())
            .collect()
    }
}