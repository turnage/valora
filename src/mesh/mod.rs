use color::{BlendMode};
use poly::Poly;
use generators::{SpawnCfg, Spawner};
use tween::Tween;
use palette::Colora;

#[derive(Debug, Clone, Copy)]
pub enum DrawMode {
    Fill,
    Stroke { thickness: f32 },
}

#[derive(Clone, Debug)]
pub struct Mesh {
    pub src: Poly,
    pub color: Colora,
    pub blend_mode: BlendMode,
    pub draw_mode: DrawMode,
    pub scale: Tween,
    pub origin_y: Tween,
    pub origin_x: Tween,
    pub rotation: Tween,
}

pub struct Instancer {
    pub src: Mesh,
    pub instances: Vec<Mesh>
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        let src = src.into();
        let origin = src.center();
        Self {
            src,
            color: Colora::rgb(1.0, 0.0, 0.0, 1.0),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
            scale: Tween::Constant(1.0),
            origin_y: Tween::Constant(origin.y),
            origin_x: Tween::Constant(origin.x),
            rotation: Tween::Constant(0.0),
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

with!(with_color, color, Colora);
with!(with_blend_mode, blend_mode, BlendMode);
with!(with_draw_mode, draw_mode, DrawMode);
with!(with_scale, scale, Tween);
with!(with_origin_y, origin_y, Tween);
with!(with_origin_x, origin_x, Tween);
with!(with_rotation, rotation, Tween);

impl Spawner<Mesh> for Mesh {
    fn spawn(&self, cfg: SpawnCfg) -> Self {
        let instance = self.clone();
        Self {
            src: instance.src.place(cfg.point),
            ..instance
        }
    }
}
