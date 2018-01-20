mod programs;
pub mod shaders;
mod tessellation;
mod render;

pub use self::shaders::*;
pub use self::tessellation::*;
pub use self::render::Render;

use color::BlendMode;
use errors::Result;
use glium::{glutin, Blend, BlitTarget, Display, IndexBuffer, Rect, Surface, VertexBuffer};
use glium::backend::{Context, Facade};
use glium::index::PrimitiveType;
use glium::texture::texture2d::Texture2d;
use glium::uniforms::MagnifySamplerFilter;
use glium::framebuffer::SimpleFrameBuffer;
use mesh::{DrawMode, Mesh};
use palette::Colora;
use poly::Point;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use self::programs::Library;

pub struct Gpu {
    display: Display,
    library: Library,
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

    pub fn draw(&self, frame: usize, cmds: Vec<(&GpuShader, &GpuMesh)>) -> Result<()> {
        let mut surface = self.display.draw();
        for &(ref shader, ref mesh) in cmds.iter() {
            shader.draw(&self.library, frame, &mut surface, mesh, None)?;
        }
        surface.finish()?;
        Ok(())
    }

    pub fn draw_to_texture(
        &self,
        textures: [&Texture2d; 2],
        frame: usize,
        cmds: Vec<(&GpuShader, &GpuMesh)>,
    ) -> Result<()> {
        let mut surfaces = [textures[0].as_surface(), textures[1].as_surface()];
        let double_buffer = cmds.iter().any(|cmd| match cmd {
            &(&GpuShader::Custom(_), _) => true,
            _ => false,
        });
        for (i, &(ref shader, ref mesh)) in cmds.iter().enumerate() {
            if double_buffer {
                shader.draw(
                    &self.library,
                    frame,
                    &mut surfaces[1],
                    mesh,
                    Some(textures[0]),
                )?;
            }
            shader.draw(
                &self.library,
                frame,
                &mut surfaces[0],
                mesh,
                Some(textures[1]),
            )?;
        }
        Ok(())
    }

    pub fn save_frame(&self, filename: &str) -> Result<()> {
        use glium::texture::RawImage2d;
        use image::{DynamicImage, ImageBuffer, ImageFormat};
        use std::fs::File;

        let image: RawImage2d<u8> = self.display.read_front_buffer();
        let image =
            ImageBuffer::from_raw(image.width, image.height, image.data.into_owned()).unwrap();
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
                    destination: LinearBlendingFactor::One,
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
    pub root_center: [f32; 2],
    pub center: [f32; 2],
    pub scale: f32,
    pub rotation: f32,
    pub vertices: Rc<VertexBuffer<GpuVertex>>,
    pub indices: Rc<IndexBuffer<u32>>,
    pub blend: Blend,
}

impl Factory<Mesh> for GpuMesh {
    fn produce(spec: Mesh, gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = match spec.draw_mode {
            DrawMode::Fill => tessellate_fill(&spec.src, spec.colorer)?,
            DrawMode::Stroke { thickness } => {
                tessellate_stroke(&spec.src, thickness, spec.colorer)?
            }
        };
        let center = GpuVertex::fix_point(spec.src.center());
        Ok(GpuMesh {
            center: [center.x, center.y],
            root_center: [center.x, center.y],
            scale: 1.0,
            rotation: 0.0,
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
