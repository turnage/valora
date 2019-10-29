use crate::amicola::{
    geo::Polygon,
    raster::regions::{RegionList, ShadeCommand},
    Element,
    RasterMethod,
    Result,
    UniformBuffer,
};
use glium::{
    backend::glutin::headless::Headless,
    implement_vertex,
    index::PrimitiveType,
    texture::{texture2d::Texture2d, MipmapsOption, UncompressedFloatFormat},
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
use std::{convert::TryFrom, rc::Rc};

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub vpos: [f32; 2],
    pub vcol: [f32; 4],
}

implement_vertex!(GpuVertex, vpos, vcol);

pub const VERTEX_SHADER: &str = include_str!("../../shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("../../shaders/default.frag");

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

    pub fn default_program(&self) -> Rc<Program> { self.program.clone() }

    pub fn build_texture(&self, width: u32, height: u32) -> Result<Texture2d> {
        Ok(Texture2d::empty_with_format(
            self.ctx.as_ref(),
            UncompressedFloatFormat::F32F32F32F32,
            MipmapsOption::NoMipmap,
            width,
            height,
        )?)
    }

    pub fn build_shader(&self, source: &str) -> Result<Program> {
        Ok(Program::from_source(
            self.ctx.as_ref(),
            VERTEX_SHADER,
            source,
            None,
        )?)
    }

    pub fn draw_to_texture(&self, cmd: GpuCommand) -> Result<()> {
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

    pub fn build_buffers(
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
                            .shade_commands()
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
