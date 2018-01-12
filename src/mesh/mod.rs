use color::{BlendMode, Colorer};
use poly::Poly;
use generators::{SpawnCfg, Spawner};

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
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        Self {
            src: src.into(),
            colorer: Colorer::empty(),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
        }
    }
}

impl Mesh {
    pub fn with_colorer(self, colorer: Colorer) -> Self {
        Self { colorer, ..self }
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
