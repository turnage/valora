use color::BlendMode;
use generators::spawner::{SpawnCfg, SpawnSrc, Spawner};
use mesh::{DrawMode, Mesh};
use palette::Colora;
use poly::{Point, Poly};
use rand::Rng;
use transforms::warp::*;

#[derive(Clone, Copy, Debug)]
pub struct WaterColorCfg {
    pub layers: usize,
    pub spread: f32,
    pub depth: usize,
    pub color: Colora,
    pub draw_mode: DrawMode,
    pub blend_mode: BlendMode,
    pub anchor_layer: bool,
    pub duplicate_depth: usize,
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
            duplicate_depth: 5,
            uniform_spread: false,
            subdivides_per: 1,
        }
    }
}

enum WaterColorSrc {
    /// An unmodified polygon from which to build a distinct layer.
    Base(Poly),
    /// An anchor layer already warped; just needs some distinction warps.
    Anchor(Poly),
}

pub struct WaterColor {
    cfg: WaterColorCfg,
    src: WaterColorSrc,
    custom_factors: Option<Vec<f32>>,
    spawn_point: Point,
}

impl WaterColor {
    pub fn new<R: Rng>(src: Poly, cfg: &WaterColorCfg, rng: &mut R) -> Self {
        let custom_factors: Option<Vec<f32>> = if cfg.uniform_spread {
            None
        } else {
            Some(
                src.vertices()
                    .into_iter()
                    .map(|_| rng.gen_range(0.5, 1.0))
                    .collect(),
            )
        };
        Self {
            spawn_point: src.center(),
            src: if cfg.anchor_layer {
                WaterColorSrc::Anchor(WaterColor::warp(src, &cfg, &custom_factors, rng))
            } else {
                WaterColorSrc::Base(src)
            },
            custom_factors,
            cfg: WaterColorCfg {
                spread: cfg.spread * cfg.subdivides_per.pow(cfg.depth as u32) as f32,
                ..*cfg
            },
        }
    }

    fn warp<R: Rng>(
        src: Poly,
        cfg: &WaterColorCfg,
        custom_factors: &Option<Vec<f32>>,
        rng: &mut R,
    ) -> Poly {
        use pipes::iterate;

        let mut src = src;
        for _ in 0..(cfg.depth) {
            src = warp(
                src,
                &WarpCfg {
                    variance: cfg.spread,
                    custom_factors: custom_factors.clone().unwrap_or(Vec::new()),
                    expansion: WarpExpansion::Outward,
                    ..WarpCfg::default()
                },
                rng,
            );
            src = iterate(src, cfg.subdivides_per, |src| src.subdivide_edges());
        }
        src
    }
}

impl Spawner<Mesh> for WaterColor {
    fn spawn(&self, cfg: SpawnCfg) -> Mesh {
        let src = match self.src {
            WaterColorSrc::Base(ref src) => {
                WaterColor::warp(src.clone(), &self.cfg, &self.custom_factors, cfg.rng)
            }
            WaterColorSrc::Anchor(ref src) => WaterColor::warp(
                src.clone(),
                &WaterColorCfg {
                    subdivides_per: 0,
                    depth: self.cfg.duplicate_depth,
                    ..self.cfg
                },
                &self.custom_factors,
                cfg.rng,
            ),
        };
        Mesh::from(src.place(cfg.point))
            .with_color(self.cfg.color)
            .with_draw_mode(self.cfg.draw_mode)
            .with_blend_mode(self.cfg.blend_mode)
    }
}

impl SpawnSrc for WaterColor {
    fn spawn_points(&self) -> Vec<Point> {
        (0..self.cfg.layers)
            .into_iter()
            .map(|_| self.spawn_point)
            .collect()
    }
}
