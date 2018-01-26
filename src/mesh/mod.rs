use color::BlendMode;
use poly::{Point, Poly};
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
    pub scale: Tween<f32>,
    pub pos: Tween<Point>,
    pub rotation: Tween<f32>,
    pub origin: Point,
}

impl MeshTransforms {
    pub fn snapshot(&self, last: &MeshSnapshot, frame: usize) -> MeshSnapshot {
        MeshSnapshot {
            color: last.color,
            scale: self.scale.tween(last, frame),
            pos: self.pos.tween(last, frame),
            rotation: self.rotation.tween(last, frame),
            origin: last.origin,
        }
    }
}

pub struct MeshSnapshot {
    pub color: Colora,
    pub scale: f32,
    pub pos: Point,
    pub rotation: f32,
    pub origin: Point,
}

#[derive(Clone, Debug)]
pub struct Instancer {
    pub src: Mesh,
    pub instances: Vec<MeshTransforms>,
}

impl<T: Into<Poly>> From<T> for Mesh {
    fn from(src: T) -> Self {
        let src = src.into();
        let pos = src.center();
        Self {
            src,
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
            transforms: MeshTransforms {
                color: Colora::rgb(1.0, 0.0, 0.0, 1.0),
                scale: Tween::Constant(1.0),
                pos: Tween::Constant(pos),
                rotation: Tween::Constant(0.0),
                origin: pos,
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
with_transform!(with_scale, scale, Tween<f32>);
with_transform!(with_pos, pos, Tween<Point>);
with_transform!(with_rotation, rotation, Tween<f32>);

impl Spawner<Mesh> for Mesh {
    fn spawn(&self, cfg: SpawnCfg) -> Self {
        let instance = self.clone();
        Self {
            src: instance.src.place(cfg.point),
            ..instance
        }
    }
}
