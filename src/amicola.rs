//! A CPU rasterizer for fine art.

mod geo;
mod raster;

use derive_more::DebugCustom;
use glium::{uniforms::Uniforms, Program};
use rental::*;
use std::rc::Rc;

pub use self::{
    geo::{Error, Polygon, V2, V4},
    raster::{
        gpu_raster::{GpuTarget, VERTEX_SHADER},
        surface::{FinalBuffer, Surface},
    },
};
pub use glium::uniforms::UniformValue;

pub trait RasterTarget {
    fn clear(&mut self);
    fn raster(&mut self, element: Element);
    fn flush(&mut self);
}

/// The method by which the rasterizer will raster the vector path.
#[derive(Debug, Clone, Copy)]
pub enum RasterMethod {
    /// In fill method, the rasterizer will treat all the area inside the path as part of the
    /// rastered area. In this method, paths are assumed to be closed.
    Fill,
    /// In stroke method, the rasterizer will treat the area immediately adjacent the path within
    /// the given thickness as part of the rastered area. In this method, paths are assumed to be
    /// open.
    Stroke(f32),
}

#[derive(Default, Clone)]
pub struct UniformBuffer {
    uniforms: Vec<(String, UniformValue<'static>)>,
}

impl UniformBuffer {
    pub fn push(&mut self, name: String, value: UniformValue<'static>) {
        self.uniforms.push((name, value));
    }
}

impl Uniforms for UniformBuffer {
    fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, mut f: F) {
        for (name, value) in &self.uniforms {
            f(name.as_str(), *value);
        }
    }
}

#[derive(Clone)]
pub struct Glsl {
    pub program: Rc<Program>,
    pub uniforms: UniformBuffer,
}

/// The method by which the rasterizer will generate a color for a pixel which is part of the fill
/// or stroke of a vector path.
#[derive(DebugCustom, Clone)]
pub enum Shader {
    /// Shades the path with a solid color.
    #[debug(fmt = "Solid shader.")]
    Solid,
    /// Shades the path with a custom shader program and uniforms.
    #[debug(fmt = "Custom shader.")]
    Glsl(Glsl),
}

/// A rasterable element in a composition.
#[derive(Debug)]
pub struct Element {
    pub path: Vec<V2>,
    pub color: V4,
    pub raster_method: RasterMethod,
    pub shader: Shader,
}
