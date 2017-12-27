mod programs;
mod shaders;

pub use self::shaders::*;

use errors::Result;
use geom::Point;
use glium::{glutin, Surface, Display, DrawParameters, Frame, IndexBuffer, Program, VertexBuffer};
use glium::{index::IndicesSource, vertex::MultiVerticesSource};
use glium::backend::{Context, Facade};
use glium::index::PrimitiveType::TrianglesList;
use glium::uniforms::Uniforms;
use palette::Colora;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use tessellation::Tessellate;
use mesh::Mesh;

pub struct Gpu {
    display: Display,
    programs: HashMap<&'static str, Rc<Program>>,
}

impl Gpu {
    pub const PROGRAM_DEFAULT: &'static str = "default";
    pub const PROGRAM_TEXTURE: &'static str = "texture";

    pub fn new(size: u32) -> Result<(Self, glutin::EventsLoop)> {
        let events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size);
        let context = glutin::ContextBuilder::new()
            .with_multisampling(16)
            .with_vsync(true);
        let display = Display::new(window, context, &events_loop)?;

        let programs = hashmap!(
            Self::PROGRAM_DEFAULT => Rc::new(programs::load(&programs::PROGRAM_SPEC_DEFAULT, &display)?),
            Self::PROGRAM_TEXTURE => Rc::new(programs::load(&programs::PROGRAM_SPEC_TEXTURE, &display)?)
        );
        Ok((Self { display, programs }, events_loop))
    }

    pub fn program(&self, id: &'static str) -> Option<Rc<Program>> {
        self.programs.get(id).map(|p| (*p).clone())
    }

    pub fn screen(&self) -> Target { self.display.draw().into() }

    pub fn save_frame(&self, filename: &str) -> Result<()> {
        use glium::texture::RawImage2d;
        use image::{DynamicImage, ImageBuffer, ImageFormat};
        use std::fs::File;

        let image: RawImage2d<u8> = self.display.read_front_buffer();
        let image = ImageBuffer::from_raw(image.width, image.height, image.data.into_owned()).unwrap();
        let image = DynamicImage::ImageRgba8(image).flipv();
        let mut output = File::create(format!("{}.png", filename))?;
        image.save(&mut output, ImageFormat::PNG).unwrap();
        Ok(())
    }

    pub fn events(mut events_loop: glutin::EventsLoop) -> Option<(glutin::EventsLoop, Vec<glutin::WindowEvent>)> {
        let mut terminate = false;
        let mut events = Vec::new();
        events_loop.poll_events(|event| {
            use glium::glutin::WindowEvent::*;
            match event {
                glutin::Event::WindowEvent { window_id: _, event } => {
                    match event {
                        glutin::WindowEvent::KeyboardInput {
                            input: glutin::KeyboardInput {
                                virtual_keycode: Some(glutin::VirtualKeyCode::Escape), ..
                            },
                            ..
                        } |
                        Closed => terminate = true,
                        _ => {
                            events.push(event);
                        }
                    }
                }
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
    fn deref(&self) -> &Display { &self.display }
}

impl Facade for Gpu {
    fn get_context(&self) -> &Rc<Context> { &self.display.get_context() }
}

impl DerefMut for Gpu {
    fn deref_mut(&mut self) -> &mut Display { &mut self.display }
}

pub trait Factory<Spec>: Sized {
    fn produce(spec: &Spec, gpu: Rc<Gpu>) -> Result<Self>;
}

#[derive(Copy, Clone)]
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

implement_vertex!(GpuVertex, position, color);

impl GpuVertex {
    const WORLD_OFFSET: f32 = 1.0;
    const WORLD_FACTOR: f32 = 2.0;

    pub fn fix_point(point: Point) -> Point {
        Point { x: Self::fix_coord(point.x), y: Self::fix_coord(point.y) }
    }

    // OpenGL places the origin in the center of the screen. We rescale
    // and offset vertices one world unit so the origin is in the bottom
    // left, and y and x point up and right respectively. If you think
    // it should be done differently, you are wrong.
    fn fix_coord(coord: f32) -> f32 { (coord * Self::WORLD_FACTOR) - Self::WORLD_OFFSET }
}

impl From<(Point, Colora)> for GpuVertex {
    fn from((point, color): (Point, Colora)) -> Self {
        use palette::Blend;

        let point = Self::fix_point(point);
        let ca = Colora { alpha: 1.0, ..color };
        let cp = ca.into_premultiplied();
        GpuVertex { position: [point.x, point.y], color: [cp.red, cp.green, cp.blue, color.alpha] }
    }
}

#[derive(Clone)]
pub struct GpuMesh {
    pub vertices: Rc<VertexBuffer<GpuVertex>>,
    pub indices: Rc<IndexBuffer<u32>>,
}

impl<T: Tessellate + Clone> Factory<Mesh<T>> for GpuMesh {
    fn produce(spec: &Mesh<T>, gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = spec.src.tessellate()?;
        Ok(GpuMesh {
               vertices: Rc::new(VertexBuffer::new(gpu.as_ref(),
                                                   tessellation
                                                       .vertices
                                                       .iter()
                                                       .map(|tv| {
                                                                let point = (*tv).into();
                                                                (point, spec.colorer.color(point))
                                                                    .into()
                                                            })
                                                       .collect::<Vec<GpuVertex>>()
                                                       .as_slice())?),
               indices: Rc::new(IndexBuffer::new(gpu.as_ref(),
                                                 TrianglesList,
                                                 tessellation.indices.as_slice())?),
           })
    }
}

pub enum Target {
    Screen(Frame),
}

impl From<Frame> for Target {
    fn from(frame: Frame) -> Self { Target::Screen(frame) }
}

impl Target {
    pub fn draw_all(mut self, cmds: Vec<(Rc<Shader>, GpuMesh)>) -> Result<()> {
        self.clear();
        for (shader, mesh) in cmds {
            shader.draw(&mut self, mesh)?;
        }
        self.finish()
    }

    pub fn draw<'a, 'b, 'v, V, I, U>(&mut self,
                                     vb: V,
                                     ib: I,
                                     program: &Program,
                                     uniforms: &U,
                                     draw_parameters: &DrawParameters)
                                     -> Result<()>
        where V: MultiVerticesSource<'b>,
              I: Into<IndicesSource<'a>>,
              U: Uniforms
    {
        use glium::Surface;
        match *self {
            Target::Screen(ref mut frame) => frame.draw(vb, ib, program, uniforms, draw_parameters).map_err(Into::into),
        }
    }

    fn clear(&mut self) {
        match *self {
            Target::Screen(ref mut frame) => frame.clear_color(0.0, 0.0, 0.0, 1.0),
        }
    }

    fn finish(self) -> Result<()> {
        match self {
            Target::Screen(frame) => frame.finish().map_err(Into::into),
        }
    }
}