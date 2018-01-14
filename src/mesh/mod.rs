use color::{BlendMode, Colorer};
use poly::Poly;
use generators::{SpawnCfg, Spawner};
use tween::Tween;

#[derive(Debug, Clone, Copy)]
pub enum DrawMode {
    Fill,
    Stroke { thickness: f32 },
}

#[derive(Clone)]
pub struct Mesh {
    pub src: Poly,
    pub colorer: Colorer,
    pub blend_mode: BlendMode,
    pub draw_mode: DrawMode,
    pub scale_tween: Option<Tween>,
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        Self {
            src: src.into(),
            colorer: Colorer::empty(),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
            scale_tween: None,
        }
    }
}

impl Mesh {
    pub fn with_colorer(self, colorer: Colorer) -> Self {
        Self { colorer, ..self }
    }

    pub fn with_blend_mode(self, blend_mode: BlendMode) -> Self {
        Self { blend_mode, ..self }
    }

    pub fn with_draw_mode(self, draw_mode: DrawMode) -> Self {
        Self { draw_mode, ..self }
    }

    pub fn with_scale_tween(self, scale_tween: Tween) -> Self {
        Self {
            scale_tween: Some(scale_tween),
            ..self
        }
    }
}

impl Spawner<Mesh> for Mesh {
    fn spawn(&self, cfg: SpawnCfg) -> Self {
        let instance = self.clone();
        Self {
            src: instance.src.place(cfg.point),
            ..instance
        }
    }
}
