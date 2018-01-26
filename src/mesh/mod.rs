use color::BlendMode;
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
    pub blend_mode: BlendMode,
    pub draw_mode: DrawMode,
    pub transforms: MeshTransforms,
}

#[derive(Clone, Debug)]
pub struct MeshTransforms {
    pub color: Colora,
    pub scale: Tween,
    pub origin_y: Tween,
    pub origin_x: Tween,
    pub rotation: Tween,
}

#[derive(Clone, Debug)]
pub struct Instancer {
    pub src: Mesh,
    pub instances: Vec<MeshTransforms>,
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        let src = src.into();
        let origin = src.center();
        Self {
            src,
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
            transforms: MeshTransforms {
                color: Colora::rgb(1.0, 0.0, 0.0, 1.0),
                scale: Tween::Constant(1.0),
                origin_y: Tween::Constant(origin.y),
                origin_x: Tween::Constant(origin.x),
                rotation: Tween::Constant(0.0),
            },
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

macro_rules! with_transform {
    ($f:ident, $field:ident, $type:ty) => {
        impl Mesh {
            pub fn $f(mut self, $field: $type) -> Self {
                self.transforms.$field = $field;
                self
            }
        }
    }
}

with!(with_blend_mode, blend_mode, BlendMode);
with!(with_draw_mode, draw_mode, DrawMode);

with_transform!(with_color, color, Colora);
with_transform!(with_scale, scale, Tween);
with_transform!(with_origin_y, origin_y, Tween);
with_transform!(with_origin_x, origin_x, Tween);
with_transform!(with_rotation, rotation, Tween);

impl Spawner<Mesh> for Mesh {
    fn spawn(&self, cfg: SpawnCfg) -> Self {
        let instance = self.clone();
        Self {
            src: instance.src.place(cfg.point),
            ..instance
        }
    }
}
