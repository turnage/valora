mod programs;
pub mod shaders;
mod tessellation;
pub mod render;

pub use self::shaders::*;
pub use self::tessellation::*;

use color::BlendMode;
use errors::Result;
use glium::{glutin, Blend, Display, IndexBuffer, Surface, VertexBuffer};
use glium::backend::{Context, Facade};
use glium::index::PrimitiveType;
use glium::texture::texture2d::Texture2d;
use mesh::Mesh;
use poly::Point;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use self::programs::Library;
use self::render::*;
use itertools::Itertools;

pub struct Gpu {
    pub display: Display,
    pub library: Library,
}

impl Gpu {
    pub fn new(size: u32) -> Result<(Self, glutin::EventsLoop)> {
        let events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size);
        let context = glutin::ContextBuilder::new()
            .with_multisampling(2)
            .with_vsync(true);
        let display = Display::new(window, context, &events_loop)?;

        let library = programs::load_library(&display)?;
        Ok((Self { display, library }, events_loop))
    }

    pub fn save_frame(&self, filename: &str) -> Result<()> {
        use glium::texture::RawImage2d;
        use image::{DynamicImage, ImageBuffer, ImageFormat};
        use std::fs::File;

        let image: RawImage2d<u8> = self.display.read_front_buffer();
        let image_data: Vec<u8> = image.data.into_owned();
        let image = ImageBuffer::from_raw(image.width, image.height, image_data).unwrap();
        let image = DynamicImage::ImageRgba8(image).flipv();
        let mut output = File::create(format!("{}.png", filename))?;
        image.save(&mut output, ImageFormat::PNG).unwrap();
        Ok(())
    }

    pub fn events(
        mut events_loop: glutin::EventsLoop,
    ) -> Option<(glutin::EventsLoop, Vec<glutin::WindowEvent>)> {
        let mut terminate = false;
        let mut events = Vec::new();
        events_loop.poll_events(|event| {
            use glium::glutin::WindowEvent::*;
            match event {
                glutin::Event::WindowEvent {
                    window_id: _,
                    event,
                } => match event {
                    glutin::WindowEvent::KeyboardInput {
                        input:
                            glutin::KeyboardInput {
                                virtual_keycode: Some(glutin::VirtualKeyCode::Escape),
                                ..
                            },
                        ..
                    }
                    | Closed => terminate = true,
                    _ => {
                        events.push(event);
                    }
                },
                _ => (),
            }
        });
        match terminate {
            true => None,
            false => Some((events_loop, events)),
        }
    }
}

impl Deref for Gpu {
    type Target = Display;
    fn deref(&self) -> &Display {
        &self.display
    }
}

impl Facade for Gpu {
    fn get_context(&self) -> &Rc<Context> {
        &self.display.get_context()
    }
}

impl DerefMut for Gpu {
    fn deref_mut(&mut self) -> &mut Display {
        &mut self.display
    }
}

pub trait Factory<Spec>: Sized {
    fn produce(spec: Spec, gpu: Rc<Gpu>) -> Result<Self>;
}

#[derive(Debug, Copy, Clone)]
pub struct GpuBareVertex {
    pub vertex_position: [f32; 2],
}

implement_vertex!(GpuBareVertex, vertex_position);

impl From<Point> for GpuBareVertex {
    fn from(point: Point) -> Self {
        GpuBareVertex {
            vertex_position: [point.x, point.y],
        }
    }
}

impl From<BlendMode> for Blend {
    fn from(src: BlendMode) -> Self {
        use glium::{BlendingFunction, LinearBlendingFactor};
        match src {
            BlendMode::Normal => Blend::alpha_blending(),
            BlendMode::Add => Blend {
                color: BlendingFunction::Addition {
                    source: LinearBlendingFactor::One,
                    destination: LinearBlendingFactor::One,
                },
                alpha: BlendingFunction::Addition {
                    source: LinearBlendingFactor::SourceAlpha,
                    destination: LinearBlendingFactor::OneMinusDestinationAlpha,
                },
                constant_value: (0.0, 0.0, 0.0, 0.0),
            },
            BlendMode::Subtract => Blend {
                color: BlendingFunction::Subtraction {
                    source: LinearBlendingFactor::One,
                    destination: LinearBlendingFactor::SourceAlpha,
                },
                alpha: BlendingFunction::Addition {
                    source: LinearBlendingFactor::SourceAlpha,
                    destination: LinearBlendingFactor::DestinationAlpha,
                },
                constant_value: (0.0, 0.0, 0.0, 0.0),
            },
            BlendMode::MaskOpaque => Blend {
                color: BlendingFunction::Addition {
                    source: LinearBlendingFactor::DestinationAlpha,
                    destination: LinearBlendingFactor::Zero,
                },
                alpha: BlendingFunction::Addition {
                    source: LinearBlendingFactor::Zero,
                    destination: LinearBlendingFactor::SourceAlpha,
                },
                constant_value: (0.0, 0.0, 0.0, 0.0),
            },
            BlendMode::MaskTransparent => Blend {
                color: BlendingFunction::Addition {
                    source: LinearBlendingFactor::OneMinusDestinationAlpha,
                    destination: LinearBlendingFactor::DestinationAlpha,
                },
                alpha: BlendingFunction::Addition {
                    source: LinearBlendingFactor::SourceAlpha,
                    destination: LinearBlendingFactor::SourceAlpha,
                },
                constant_value: (0.0, 0.0, 0.0, 0.0),
            },
        }
    }
}

pub struct GpuMesh<V: Copy> {
    pub vertices: Rc<VertexBuffer<V>>,
    pub indices: Rc<IndexBuffer<u32>>,
}

impl Factory<Mesh> for GpuMesh<GpuBareVertex> {
    fn produce(spec: Mesh, gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = tessellate(&spec)?;
        Ok(GpuMesh {
            vertices: Rc::new(VertexBuffer::new(
                gpu.as_ref(),
                &tessellation
                    .vertices
                    .into_iter()
                    .map(Into::into)
                    .collect::<Vec<GpuBareVertex>>(),
            )?),
            indices: Rc::new(IndexBuffer::new(
                gpu.as_ref(),
                PrimitiveType::TrianglesList,
                tessellation.indices.as_slice(),
            )?),
        })
    }
}

impl<'a> Factory<&'a [Mesh]> for GpuMesh<GpuBatchVertex> {
    fn produce(specs: &[Mesh], gpu: Rc<Gpu>) -> Result<Self> {
        let maybe_tessellation: Result<(Vec<GpuBatchVertex>, Vec<u32>)> = specs
            .iter()
            .enumerate()
            .map(|(i, spec)| {
                let tessellation = tessellate(spec)?;
                let batch_vertices = tessellation
                    .vertices
                    .into_iter()
                    .map(|v| GpuBatchVertex {
                        vertex_position: (v.x, v.y),
                        mesh_index: i as u32,
                    })
                    .collect::<Vec<GpuBatchVertex>>();
                Ok((batch_vertices, tessellation.indices))
            })
            .fold_results(
                (Vec::new(), Vec::new()),
                |(mut vertices, mut indices), (mut new_vertices, new_indices)| {
                    let index_offset = vertices.len() as u32;
                    vertices.append(&mut new_vertices);
                    indices.extend(new_indices.into_iter().map(|i| i + index_offset));
                    (vertices, indices)
                },
            );
        let (vertices, indices): (Vec<GpuBatchVertex>, Vec<u32>) = maybe_tessellation?;
        Ok(GpuMesh {
            vertices: Rc::new(VertexBuffer::new(gpu.as_ref(), vertices.as_slice())?),
            indices: Rc::new(IndexBuffer::new(
                gpu.as_ref(),
                PrimitiveType::TrianglesList,
                indices.as_slice(),
            )?),
        })
    }
}
