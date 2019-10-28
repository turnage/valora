//! A CPU rasterizer for fine art.

mod geo;
mod raster;

use derive_more::DebugCustom;
use failure::{Error, Fail};
use glium::{uniforms::Uniforms, Program};
use rental::*;
use std::rc::Rc;
use rand::random();
use itertools::Itertools;

pub use self::{
    geo::{Error, Polygon, V2, V4},
    raster::{
        gpu::{Gpu, GpuCommand},
        surface::{FinalBuffer, Surface},
    },
};
pub use glium::uniforms::UniformValue;

type Result<T> = std::result::Result<T, Error>;

mod sealed {
    use super::*;

    pub trait Seal {}

    impl Seal for FragmentShader {}
}


pub trait Shader: sealed::Seal {
    fn id(&self) -> u64;
    fn program(&self) -> &Program;
    fn uniforms(&self) -> &UniformBuffer;
}

/// A context object for the amicola rasterizer.
pub struct Amicola {
    next_resource_id: u64,
    gpu: Gpu,
}

impl Amicola {
    pub fn new() -> Result<Self> { Ok(Amicola { 
               next_resource_id: 0,
        gpu: Gpu::new()? }) }

    pub fn compile_shader(&self, source: &str) -> Result<Rc<Program>> {
        Ok(Rc::new(self.gpu.build_shader(source)?)
    }

    pub fn finish_shader(&self, program: Rc<Program>, uniform_buffer: UniformBuffer) -> Result<impl Shader> {
        Ok(FragmentShader {
            id: random(),
            program,
            uniform_buffer
        })
    }

    pub fn precompose(&self, width: u32, height: u32, elements: impl Iterator<Item=Element>) -> Result<Rc<Texture2d>> {
        let texture = self.gpu.build_texture(width, height)?;
        for (id, batch) in elements.group_by(|(_, e)| e.shader) {
            let batch = batch.peekable();
            let first = if let Some(first) = batch.peek() {
                first
            } else {
                println!("This is possible??");
                continue;
            };

            let (vertices, indices) = self.gpu.build_buffers(batch)?;
            self.draw_to_texture(GpuCommand {
                vertices,
                indices,
                texture: &texture,
                program: first.shader.progam(),
                uniforms: first.shader.uniforms()
            })?;

            texture
        }
    }
}

/// A fragment shader which can be used to shade paths.
struct FragmentShader {
    id: u64,
    program: Rc<Program>,
    uniform_buffer: UniformBuffer,
}

impl Shader for FragmentShader {
    fn id(&self) -> u64 { self.id }
    fn program(&self) -> &Program { self.progam.as_ref() }
    fn uniforms(&self) -> &UniformBuffer { &self.uniform_buffer }
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

/// A rasterable element in a composition.
#[derive(Debug)]
pub struct Element {
    pub path: Vec<V2>,
    pub color: V4,
    pub raster_method: RasterMethod,
    pub shader: Rc<dyn Shader>,
}
