//! GPU handle and types.

use crate::{
    raster::{raster_path, Method},
    uniforms::*,
    Result,
};
use glium::{
    backend::{
        glutin::{headless::Headless, Display},
        Context,
        Facade,
    },
    glutin::EventsLoop,
    implement_vertex,
    index::PrimitiveType,
    texture::{
        texture2d::Texture2d,
        texture2d_multisample::Texture2dMultisample,
        MipmapsOption,
        RawImage2d,
        UncompressedFloatFormat,
    },
    uniforms::{MagnifySamplerFilter, UniformValue, Uniforms},
    Blend,
    BlendingFunction,
    DrawParameters,
    Frame,
    IndexBuffer,
    LinearBlendingFactor,
    Program,
    Surface,
    VertexBuffer,
};
use glutin::dpi::PhysicalSize;
use itertools::Itertools;
use lyon_path::Builder;
use palette::LinSrgba;
use rand::random;
use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub vpos: [f32; 2],
    pub vcol: [f32; 4],
}

implement_vertex!(GpuVertex, vpos, vcol);

pub const VERTEX_SHADER: &str = include_str!("shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("shaders/default.frag");

struct NoUniforms;

impl Uniforms for NoUniforms {
    fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, _f: F) {}
}

#[derive(Default, Clone)]
pub struct UniformBuffer {
    user_uniforms: Option<Rc<dyn OwnedUniforms>>,
    uniforms: Vec<(String, UniformValue<'static>)>,
}

impl UniformBuffer {
    pub fn push(&mut self, name: String, value: UniformValue<'static>) {
        self.uniforms.push((name, value));
    }
}

impl Uniforms for UniformBuffer {
    fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, mut f: F) {
        if let Some(user_uniforms) = self.user_uniforms.as_ref().map(Rc::as_ref) {
            user_uniforms.visit_owned_values(&mut |name, v| {
                let uniform_value = v.into_uniform_value();
                let uniform_value: UniformValue<'a> = unsafe { std::mem::transmute(uniform_value) };
                f(name, uniform_value)
            });
        }
        for (name, value) in &self.uniforms {
            f(name.as_str(), *value);
        }
    }
}

impl<U> From<U> for UniformBuffer
where
    U: OwnedUniforms + 'static,
{
    fn from(src: U) -> Self {
        UniformBuffer {
            user_uniforms: Some(Rc::new(src)),
            uniforms: vec![],
        }
    }
}

/// A shader which can be used to shade paths.
#[derive(Clone)]
pub struct Shader {
    id: u64,
    program: Rc<Program>,
    uniforms: UniformBuffer,
}

/// A rasterable element in a composition.
pub struct Element {
    pub path: Builder,
    pub color: LinSrgba,
    pub raster_method: Method,
    pub shader: Shader,
}

pub struct DisplayFacade(Display);

impl Facade for DisplayFacade {
    fn get_context(&self) -> &Rc<Context> { self.0.get_context() }
}

pub trait FacadeExt: Facade {
    fn get_frame(&self) -> Option<Frame> { None }
}

impl FacadeExt for Headless {}

impl FacadeExt for DisplayFacade {
    fn get_frame(&self) -> Option<Frame> { Some(self.0.draw()) }
}

/// A handle to the GPU for managing buffers and shaders.
#[derive(Clone)]
pub struct Gpu {
    ctx: Rc<dyn FacadeExt>,
    program: Rc<Program>,
    height_sign: f32,
}

struct GpuCommand<'a, S> {
    pub vertices: VertexBuffer<GpuVertex>,
    pub indices: IndexBuffer<u32>,
    pub target: &'a mut S,
    pub program: &'a Program,
    pub uniforms: &'a UniformBuffer,
}

impl Gpu {
    pub(crate) fn new() -> Result<(Self, EventsLoop)> {
        let events_loop = EventsLoop::new();
        let ctx = glium::glutin::ContextBuilder::new()
            .with_multisampling(0)
            .build_headless(
                &events_loop,
                PhysicalSize {
                    width: 0.0,
                    height: 0.0,
                },
            )?;
        let ctx = Rc::new(Headless::new(ctx)?);

        let program = Rc::new(Program::from_source(
            ctx.as_ref(),
            VERTEX_SHADER,
            FRAGMENT_SHADER,
            None,
        )?);

        Ok((
            Gpu {
                program,
                ctx,
                height_sign: 1.,
            },
            events_loop,
        ))
    }

    pub(crate) fn with_window(width: u32, height: u32) -> Result<(Self, EventsLoop)> {
        let events_loop = EventsLoop::new();
        let wb = glium::glutin::WindowBuilder::new()
            .with_dimensions(glutin::dpi::LogicalSize {
                width: width as f64,
                height: height as f64,
            })
            .with_title("Hello world");
        let cb = glium::glutin::ContextBuilder::new()
            .with_srgb(false)
            .with_multisampling(8);
        let display = glium::Display::new(wb, cb, &events_loop).unwrap();
        let ctx = Rc::new(DisplayFacade(display));

        let program = Rc::new(Program::from_source(
            ctx.as_ref(),
            VERTEX_SHADER,
            FRAGMENT_SHADER,
            None,
        )?);

        Ok((
            Gpu {
                program,
                ctx,
                height_sign: -1.,
            },
            events_loop,
        ))
    }

    pub(crate) fn get_frame(&self) -> Option<Frame> { self.ctx.get_frame() }

    pub(crate) fn default_shader(&self, width: f32, height: f32) -> Shader {
        Shader {
            id: random(),
            program: self.program.clone(),
            uniforms: UniformBuffer {
                user_uniforms: None,
                uniforms: vec![
                    (String::from("width"), UniformValue::Float(width)),
                    (String::from("height"), UniformValue::Float(height)),
                ],
            },
        }
    }

    pub fn compile_glsl(&self, source: &str) -> Result<Rc<Program>> {
        Ok(Rc::new(Program::from_source(
            self.ctx.as_ref(),
            VERTEX_SHADER,
            source,
            None,
        )?))
    }

    pub fn build_shader(&self, program: Rc<Program>, uniforms: impl Into<UniformBuffer>) -> Shader {
        Shader {
            id: random(),
            program,
            uniforms: uniforms.into(),
        }
    }

    pub fn build_texture(&self, width: u32, height: u32) -> Result<Texture2dMultisample> {
        Ok(Texture2dMultisample::empty_with_format(
            self.ctx.as_ref(),
            UncompressedFloatFormat::F32F32F32F32,
            MipmapsOption::NoMipmap,
            width,
            height,
            /*samples=*/ 16,
        )?)
    }

    fn build_ram_texture(&self, width: u32, height: u32) -> Result<Texture2d> {
        Ok(Texture2d::empty_with_format(
            self.ctx.as_ref(),
            UncompressedFloatFormat::F32F32F32F32,
            MipmapsOption::NoMipmap,
            width,
            height,
        )?)
    }

    pub fn read_to_ram(&self, texture: &Texture2dMultisample) -> Result<RawImage2d<u8>> {
        let (width, height) = texture.dimensions();
        let target = self.build_ram_texture(width, height)?;
        texture.as_surface().blit_color(
            &glium::Rect {
                bottom: 0,
                left: 0,
                width,
                height,
            },
            &target.as_surface(),
            &glium::BlitTarget {
                bottom: 0,
                left: 0,
                width: width as i32,
                height: height as i32,
            },
            MagnifySamplerFilter::Linear,
        );
        Ok(target.read())
    }

    pub fn render(
        &self,
        width: u32,
        height: u32,
        elements: impl IntoIterator<Item = Element>,
        target: &mut impl Surface,
    ) -> Result<()> {
        let elements = elements.into_iter();
        for (_id, batch) in &elements.group_by(|e| e.shader.id) {
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
            first.uniforms.push(
                String::from("height_sign"),
                UniformValue::Float(self.height_sign),
            );

            let (_, cpu_vertices, cpu_indices) = batch
                .try_fold::<_, _, Result<(u32, Vec<GpuVertex>, Vec<u32>)>>(
                    (0, vec![], vec![]),
                    |(idx, mut vertices, mut indices), element| {
                        let (mut new_vertices, new_indices) =
                            raster_path(element.path, element.raster_method, element.color)?;
                        vertices.append(&mut new_vertices);
                        indices.extend(new_indices.into_iter().map(|i| i + idx));
                        Ok((vertices.len() as u32, vertices, indices))
                    },
                )?;

            let vertices = VertexBuffer::new(self.ctx.as_ref(), cpu_vertices.as_slice())?;
            let indices = IndexBuffer::new(
                self.ctx.as_ref(),
                PrimitiveType::TrianglesList,
                cpu_indices.as_slice(),
            )?;

            self.draw_to_texture(GpuCommand {
                indices,
                vertices,
                target,
                program: first.program.as_ref(),
                uniforms: &first.uniforms,
            })?;
        }

        Ok(())
    }

    fn draw_to_texture<S: Surface>(&self, cmd: GpuCommand<S>) -> Result<()> {
        Ok(cmd.target.draw(
            &cmd.vertices,
            &cmd.indices,
            cmd.program,
            cmd.uniforms,
            &DrawParameters {
                blend: Blend {
                    color: BlendingFunction::Addition {
                        source: LinearBlendingFactor::SourceAlpha,
                        destination: LinearBlendingFactor::OneMinusSourceAlpha,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::One,
                        destination: LinearBlendingFactor::OneMinusSourceAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                },
                line_width: Some(1.0),
                multisampling: true,
                dithering: false,
                smooth: Some(glium::draw_parameters::Smooth::Nicest),
                ..Default::default()
            },
        )?)
    }
}
