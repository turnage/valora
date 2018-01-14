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
    pub scale: Tween,
    pub origin_y: Tween,
    pub origin_x: Tween,
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        let src = src.into();
        let origin = src.center();
        Self {
            src,
            colorer: Colorer::empty(),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
            scale: Tween::Constant(1.0),
            origin_y: Tween::Constant(origin.y),
            origin_x: Tween::Constant(origin.x),
        }
    }
}

macro_rules! with {
    ($f:ident, $field:ident, $type:ty) => {
        impl Mesh {
            pub fn $f(self, $field: $type) -> Self {
                Self { $field, ..self }
            }
        }
    }
}

with!(with_colorer, colorer, Colorer);
with!(with_blend_mode, blend_mode, BlendMode);
with!(with_draw_mode, draw_mode, DrawMode);
with!(with_scale, scale, Tween);
with!(with_origin_y, origin_y, Tween);
with!(with_origin_x, origin_x, Tween);

impl Spawner<Mesh> for Mesh {
    fn spawn(&self, cfg: SpawnCfg) -> Self {
        let instance = self.clone();
        Self {
            src: instance.src.place(cfg.point),
            ..instance
        }
    }
}
