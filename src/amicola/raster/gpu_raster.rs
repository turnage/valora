use crate::amicola::{
    geo::{Polygon, V4},
    raster::regions::{RegionList, ShadeCommand},
    Element,
    RasterMethod,
    RasterTarget,
    Shader,
};
use glium::{
    backend::glutin::headless::Headless,
    implement_vertex,
    index::PrimitiveType,
    texture::{texture2d::Texture2d, RawImage2d},
    uniform,
    Blend,
    BlendingFunction,
    Display,
    DrawParameters,
    Frame,
    IndexBuffer,
    LinearBlendingFactor,
    Program,
    Surface,
    VertexBuffer,
};
use glutin::{dpi::PhysicalSize, ContextBuilder, EventsLoop};
use image::{ImageBuffer, Rgba};
use itertools::{Either, Itertools};
use std::{convert::TryFrom, rc::Rc};

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub vpos: [f32; 2],
    pub vcol: [f32; 4],
}

implement_vertex!(GpuVertex, vpos, vcol);

const VERTEX_SHADER: &str = include_str!("../../shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("../../shaders/default.frag");

pub struct GpuTarget {
    surface: Rc<Texture2d>,
    ctx: Rc<Headless>,
    program: Rc<Program>,
    events_loop: EventsLoop,
    width: f32,
    height: f32,
}

impl GpuTarget {
    pub fn with_dimensions(width: u32, height: u32) -> Self {
        let events_loop = glium::glutin::EventsLoop::new();
        let ctx = glium::glutin::ContextBuilder::new()
            .with_multisampling(0)
            .build_headless(
                &events_loop,
                PhysicalSize {
                    width: width as f64,
                    height: height as f64,
                },
            )
            .expect("glutin Headless context");
        let ctx = Rc::new(Headless::new(ctx).expect("glutin Headless backend"));

        let program = Rc::new(
            Program::from_source(ctx.as_ref(), VERTEX_SHADER, FRAGMENT_SHADER, None)
                .expect("default shader"),
        );

        GpuTarget {
            surface: Rc::new(
                Texture2d::empty(ctx.as_ref(), width, height).expect("texture buffer"),
            ),
            program,
            ctx,
            events_loop,
            width: width as f32,
            height: height as f32,
        }
    }

    pub fn image(&self) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
        let raw: RawImage2d<u8> = self.surface.read();
        ImageBuffer::from_raw(self.width as u32, self.height as u32, raw.data.into_owned()).unwrap()
    }
}

impl RasterTarget for GpuTarget {
    fn clear(&mut self) { self.surface.as_surface().clear_color(1.0, 1.0, 1.0, 1.0); }

    fn flush(&mut self) {}

    fn raster(&mut self, mut element: Element) {
        match element.raster_method {
            RasterMethod::Fill => {
                let poly = match Polygon::try_from(element.path) {
                    Ok(poly) => poly,
                    // An unclosed path has no fill.
                    _ => return,
                };

                let rgba = element.color;

                let vertices = RegionList::from(poly)
                    .shade_commands()
                    .flat_map(|cmd| match cmd {
                        ShadeCommand::Boundary { x, y, coverage } => vec![
                            GpuVertex {
                                vpos: [x, y],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w * coverage],
                            },
                            GpuVertex {
                                vpos: [x + 1.0, y],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w * coverage],
                            },
                        ],
                        ShadeCommand::Span { start_x, end_x, y } => vec![
                            GpuVertex {
                                vpos: [start_x, y],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w],
                            },
                            GpuVertex {
                                vpos: [end_x, y],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w],
                            },
                        ],
                    })
                    .collect::<Vec<GpuVertex>>();
                let vertex_buffer = VertexBuffer::new(self.ctx.as_ref(), vertices.as_slice())
                    .expect("vertex buffer");

                let indices = vertices
                    .iter()
                    .enumerate()
                    .map(|(i, _)| i as u32)
                    .collect::<Vec<u32>>();
                let index_buffer = IndexBuffer::new(
                    self.ctx.as_ref(),
                    PrimitiveType::LinesList,
                    indices.as_slice(),
                )
                .expect("index buffer");

                let w = self.width;
                let h = self.height;
                let program = self.program.clone();
                self.surface
                    .as_surface()
                    .draw(
                        &vertex_buffer,
                        &index_buffer,
                        program.as_ref(),
                        &uniform! {
                            width: w,
                            height: h
                        },
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
                    )
                    .expect("draw");
            }
            _ => unimplemented!(),
        };
    }
}
