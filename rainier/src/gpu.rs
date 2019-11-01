use crate::Result;
use amicola::{Polygon, RasterMethod, RegionList, SampleDepth, ShadeCommand, V2, V4};
use glium::{
    backend::glutin::headless::Headless,
    implement_vertex,
    index::PrimitiveType,
    texture::{texture2d::Texture2d, MipmapsOption, UncompressedFloatFormat},
    uniforms::{UniformValue, Uniforms},
    Blend,
    BlendingFunction,
    DrawParameters,
    IndexBuffer,
    LinearBlendingFactor,
    Program,
    Surface,
    VertexBuffer,
};
use glutin::dpi::PhysicalSize;
use itertools::Itertools;
use rand::random;
use std::{convert::TryFrom, rc::Rc};

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub vpos: [f32; 2],
    pub vcol: [f32; 4],
}

implement_vertex!(GpuVertex, vpos, vcol);

pub const VERTEX_SHADER: &str = include_str!("shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("shaders/default.frag");

#[derive(Clone)]
pub struct Shader {
    id: u64,
    program: Rc<Program>,
    uniforms: UniformBuffer,
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
    pub sample_depth: SampleDepth,
}

pub struct Gpu {
    ctx: Rc<Headless>,
    program: Rc<Program>,
}

pub struct GpuCommand<'a> {
    pub vertices: VertexBuffer<GpuVertex>,
    pub indices: IndexBuffer<u32>,
    pub texture: &'a Texture2d,
    pub program: &'a Program,
    pub uniforms: &'a UniformBuffer,
}

impl Gpu {
    pub fn new() -> Result<Self> {
        let events_loop = glium::glutin::EventsLoop::new();
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

        Ok(Gpu { program, ctx })
    }

    pub fn default_shader(&self, width: f32, height: f32) -> Shader {
        Shader {
            id: random(),
            program: self.program.clone(),
            uniforms: UniformBuffer {
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

    pub fn build_shader(&self, program: Rc<Program>, uniforms: UniformBuffer) -> Result<Shader> {
        Ok(Shader {
            id: random(),
            program,
            uniforms,
        })
    }

    pub fn build_texture(&self, width: u32, height: u32) -> Result<Texture2d> {
        Ok(Texture2d::empty_with_format(
            self.ctx.as_ref(),
            UncompressedFloatFormat::F32F32F32F32,
            MipmapsOption::NoMipmap,
            width,
            height,
        )?)
    }

    pub fn precompose(
        &self,
        width: u32,
        height: u32,
        elements: impl Iterator<Item = Element>,
    ) -> Result<Rc<Texture2d>> {
        let texture = self.build_texture(width, height)?;
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

            let (indices, vertices) = self.build_buffers(batch)?;
            self.draw_to_texture(GpuCommand {
                indices,
                vertices,
                texture: &texture,
                program: first.program.as_ref(),
                uniforms: &first.uniforms,
            })?;
        }
        Ok(Rc::new(texture))
    }

    fn draw_to_texture(&self, cmd: GpuCommand) -> Result<()> {
        Ok(cmd.texture.as_surface().draw(
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
                multisampling: false,
                dithering: false,
                smooth: None,
                ..Default::default()
            },
        )?)
    }

    fn build_buffers(
        &self,
        mut elements: impl Iterator<Item = Element>,
    ) -> Result<(IndexBuffer<u32>, VertexBuffer<GpuVertex>)> {
        let vertices = elements
            .flat_map(|element| {
                let rgba = [
                    element.color.x,
                    element.color.y,
                    element.color.z,
                    element.color.w,
                ];
                match element.raster_method {
                    RasterMethod::Fill => {
                        let poly = match Polygon::try_from(element.path) {
                            Ok(poly) => poly,
                            // An unclosed path has no fill.
                            _ => return vec![],
                        };

                        RegionList::from(poly)
                            .shade_commands(element.sample_depth)
                            .flat_map(|cmd| match cmd {
                                ShadeCommand::Boundary { x, y, coverage } => {
                                    let rgba = {
                                        let mut copy = rgba;
                                        copy[3] *= coverage;
                                        copy
                                    };
                                    vec![
                                        GpuVertex {
                                            vpos: [x, y],
                                            vcol: rgba,
                                        },
                                        GpuVertex {
                                            vpos: [x + 1.0, y],
                                            vcol: rgba,
                                        },
                                    ]
                                }
                                ShadeCommand::Span { start_x, end_x, y } => vec![
                                    GpuVertex {
                                        vpos: [start_x, y],
                                        vcol: rgba,
                                    },
                                    GpuVertex {
                                        vpos: [end_x, y],
                                        vcol: rgba,
                                    },
                                ],
                            })
                            .collect::<Vec<GpuVertex>>()
                    }
                    _ => unimplemented!(),
                }
            })
            .collect::<Vec<GpuVertex>>();

        let vertex_buffer = VertexBuffer::new(self.ctx.as_ref(), vertices.as_slice())?;

        let indices = vertices
            .iter()
            .enumerate()
            .map(|(i, _)| i as u32)
            .collect::<Vec<u32>>();
        let index_buffer = IndexBuffer::new(
            self.ctx.as_ref(),
            PrimitiveType::LinesList,
            indices.as_slice(),
        )?;

        Ok((index_buffer, vertex_buffer))
    }
}
