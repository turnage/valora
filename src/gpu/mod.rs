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
use mesh::{Mesh};
use palette::Colora;
use poly::{Point, Poly};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use self::programs::Library;
use self::render::DrawCmd;

pub struct Gpu {
    display: Display,
    pub library: Library,
}

impl Gpu {
    pub fn new(size: u32) -> Result<(Self, glutin::EventsLoop)> {
        let events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size);
        let context = glutin::ContextBuilder::new()
            .with_multisampling(16)
            .with_vsync(true);
        let display = Display::new(window, context, &events_loop)?;
        let library = programs::load_library(&display)?;
        Ok((Self { display, library }, events_loop))
    }

    pub fn draw_simple(&self, cmd: DrawCmd) -> Result<()> {
        let mut surface = self.display.draw();
        surface.clear_color(1.0, 0.0, 0.0, 1.0);
        surface.draw(cmd.mesh.vertices.as_ref(), cmd.mesh.indices.as_ref(), &cmd.shader.program, &cmd.shader.uniforms, &Default::default())?;
        surface.finish()?;
        Ok(())
    }

    pub fn save_frame(&self, texture: &Texture2d, filename: &str) -> Result<()> {
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
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

implement_vertex!(GpuVertex, position, color);

impl GpuVertex {
    const WORLD_OFFSET: f32 = 1.0;
    const WORLD_FACTOR: f32 = 2.0;

    pub fn fix_point(point: Point) -> Point {
        Point {
            x: Self::fix_coord(point.x),
            y: Self::fix_coord(point.y),
        }
    }

    // OpenGL places the origin in the center of the screen. We rescale
    // and offset vertices one world unit so the origin is in the bottom
    // left, and y and x point up and right respectively. If you think
    // it should be done differently, you are wrong.
    fn fix_coord(coord: f32) -> f32 {
        (coord * Self::WORLD_FACTOR) - Self::WORLD_OFFSET
    }

    pub fn unfix_point(point: Point) -> Point {
        point.offset(Self::WORLD_OFFSET) / Self::WORLD_FACTOR
    }
}

impl From<(Point, Colora)> for GpuVertex {
    fn from((point, color): (Point, Colora)) -> Self {
        use palette::Blend;

        let point = Self::fix_point(point);
        let ca = Colora {
            alpha: 1.0,
            ..color
        };
        let cp = ca.into_premultiplied();
        GpuVertex {
            position: [point.x, point.y],
            color: [cp.red, cp.green, cp.blue, color.alpha],
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

#[derive(Clone)]
pub struct GpuMesh {
    pub vertices: Rc<VertexBuffer<GpuVertex>>,
    pub indices: Rc<IndexBuffer<u32>>,
    pub blend: Blend,
}

impl Factory<Mesh> for GpuMesh {
    fn produce(spec: Mesh, gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = tessellate(&spec)?;
        Ok(GpuMesh {
            vertices: Rc::new(VertexBuffer::new(
                gpu.as_ref(),
                tessellation.vertices.as_slice(),
            )?),
            indices: Rc::new(IndexBuffer::new(
                gpu.as_ref(),
                PrimitiveType::TrianglesList,
                tessellation.indices.as_slice(),
            )?),
            blend: Blend::from(spec.blend_mode),
        })
    }
}

impl<'a> Factory<&'a [Mesh]> for GpuMesh {
    fn produce(specs: &[Mesh], gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = specs.iter().map(|spec| tessellate(spec)).collect::<Result<Tessellation>>()?;
        Ok(GpuMesh {
            vertices: Rc::new(VertexBuffer::new(
                gpu.as_ref(),
                tessellation.vertices.as_slice(),
            )?),
            indices: Rc::new(IndexBuffer::new(
                gpu.as_ref(),
                PrimitiveType::TrianglesList,
                tessellation.indices.as_slice(),
            )?),
            blend: Blend::from(specs[0].blend_mode)
        })
    }
}