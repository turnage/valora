use crate::amicola::geo::{Polygon, V4};
use crate::amicola::raster::regions::{Region, RegionList, ShadeCommand};
use crate::amicola::RasterTarget;
use crate::amicola::{Element, RasterMethod, Shader};
use glium::backend::glutin::headless::Headless;
use glium::implement_vertex;
use glium::index::PrimitiveType;
use glium::texture::texture2d::Texture2d;
use glium::uniform;
use glium::Display;
use glium::IndexBuffer;
use glium::Program;
use glium::Surface;
use glium::VertexBuffer;
use glutin::dpi::LogicalSize;
use glutin::ContextBuilder;
use glutin::EventsLoop;
use itertools::{Either, Itertools};
use std::{convert::TryFrom, rc::Rc};

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub vpos: [f32; 2],
    pub vcol: [f64; 4],
}

implement_vertex!(GpuVertex, vpos, vcol);

const VERTEX_SHADER: &str = include_str!("../../shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("../../shaders/default.frag");

pub struct GpuTarget {
    surface: Texture2d,
    display: Display,
    program: Program,
    events_loop: EventsLoop,
    width: f32,
    height: f32,
}

impl GpuTarget {
    pub fn with_dimensions(width: u32, height: u32) -> Self {
        let events_loop = glium::glutin::EventsLoop::new();
        let wb = glium::glutin::WindowBuilder::new()
            .with_dimensions(LogicalSize {
                width: width as f64,
                height: height as f64,
            })
            .with_title("valora");
        let cb = glium::glutin::ContextBuilder::new();
        let display = glium::Display::new(wb, cb, &events_loop).unwrap();

        let program = Program::from_source(&display, VERTEX_SHADER, FRAGMENT_SHADER, None)
            .expect("default shader");

        GpuTarget {
            surface: Texture2d::empty(&display, width, height).expect("texture buffer"),
            program,
            display,
            events_loop,
            width: width as f32,
            height: height as f32,
        }
    }
}

impl RasterTarget for GpuTarget {
    fn raster(&mut self, mut element: Element) {
        match element.raster_method {
            RasterMethod::Fill => {
                let poly = match Polygon::try_from(element.path) {
                    Ok(poly) => poly,
                    // An unclosed path has no fill.
                    _ => return,
                };

                let rgba = match element.shader {
                    Shader::Solid(rgba) => rgba,
                };

                let vertices = RegionList::from(poly)
                    .shade_commands()
                    .flat_map(|cmd| match cmd.region {
                        Region::Boundary { x, y } => {
                            let vert = GpuVertex {
                                vpos: [x as f32, y as f32],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w * cmd.coverage],
                            };
                            vec![vert, vert]
                        }
                        Region::Fill { start_x, end_x, y } => vec![
                            GpuVertex {
                                vpos: [end_x as f32, y as f32],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w],
                            },
                            GpuVertex {
                                vpos: [start_x as f32, y as f32],
                                vcol: [rgba.x, rgba.y, rgba.z, rgba.w],
                            },
                        ],
                    })
                    .collect::<Vec<GpuVertex>>();
                let vertex_buffer =
                    VertexBuffer::new(&self.display, vertices.as_slice()).expect("vertex buffer");

                let indices = vertices
                    .iter()
                    .enumerate()
                    .map(|(i, _)| i as u32)
                    .collect::<Vec<u32>>();
                let index_buffer =
                    IndexBuffer::new(&self.display, PrimitiveType::LinesList, indices.as_slice())
                        .expect("index buffer");

                let mut frame = self.display.draw();
                frame
                    .draw(
                        &vertex_buffer,
                        &index_buffer,
                        &self.program,
                        &uniform! {
                            width: self.width,
                            height: self.height
                        },
                        &Default::default(),
                    )
                    .expect("draw");
                frame.finish();
            }
            _ => unimplemented!(),
        };
    }
}
