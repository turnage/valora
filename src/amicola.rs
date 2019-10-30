//! A CPU rasterizer for fine art.

mod geo;
mod raster;

use failure::Error;
use glium::{texture::texture2d::Texture2d, uniforms::Uniforms, Program};
use itertools::Itertools;
use rand::random;
use std::rc::Rc;

pub use self::{
    geo::{Polygon, V2, V4},
    raster::{
        gpu::{Gpu, GpuCommand},
        surface::{FinalBuffer, Surface},
    },
};
pub use glium::uniforms::UniformValue;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct Shader {
    id: u64,
    program: Rc<Program>,
    uniforms: UniformBuffer,
}

/// A context object for the amicola rasterizer.
pub struct Amicola {
    next_resource_id: u64,
    gpu: Gpu,
}

impl Amicola {
    pub fn new() -> Result<Self> {
        Ok(Amicola {
            next_resource_id: 0,
            gpu: Gpu::new()?,
        })
    }

    pub fn default_shader(&self, width: f32, height: f32) -> Shader {
        Shader {
            id: random(),
            program: self.gpu.default_program(),
            uniforms: UniformBuffer {
                uniforms: vec![
                    (String::from("width"), UniformValue::Float(width)),
                    (String::from("height"), UniformValue::Float(height)),
                ],
            },
        }
    }

    pub fn compile_glsl(&self, source: &str) -> Result<Rc<Program>> {
        Ok(Rc::new(self.gpu.build_shader(source)?))
    }

    pub fn build_shader(&self, program: Rc<Program>, uniforms: UniformBuffer) -> Result<Shader> {
        Ok(Shader {
            id: random(),
            program,
            uniforms,
        })
    }

    pub fn precompose(
        &self,
        width: u32,
        height: u32,
        elements: impl Iterator<Item = Element>,
    ) -> Result<Rc<Texture2d>> {
        let texture = self.gpu.build_texture(width, height)?;
        for (id, batch) in &elements.group_by(|e| e.shader.id) {
            let mut batch = batch.peekable();
            let mut first = if let Some(first) = batch.peek() {
                first.shader.clone()
            } else {
                println!("This is possible??");
                continue;
            };

            // TODO: reconcile conflicts between user uniforms and the defaults
            first
                .uniforms
                .push(String::from("width"), UniformValue::Float(width as f32));
            first
                .uniforms
                .push(String::from("height"), UniformValue::Float(height as f32));

            let (indices, vertices) = self.gpu.build_buffers(batch)?;
            self.gpu.draw_to_texture(GpuCommand {
                indices,
                vertices,
                texture: &texture,
                program: first.program.as_ref(),
                uniforms: &first.uniforms,
            })?;
        }
        Ok(Rc::new(texture))
    }
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
pub struct Element {
    pub path: Vec<V2>,
    pub color: V4,
    pub raster_method: RasterMethod,
    pub shader: Shader,
}
