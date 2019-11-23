//! Tools for composing generative fine art.

mod gpu;
mod raster;

pub use self::gpu::{Shader, UniformBuffer};
pub use glium::program::Program;
pub use palette::{self, Alpha, Blend, ComponentWise, Hue, IntoColor, LinSrgb, LinSrgba, Saturate};
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
pub use structopt::StructOpt;

use self::{gpu::*, raster::Method};
use failure::Error;
use glium::glutin::EventsLoop;
use image::{ImageBuffer, Rgba};
use lyon_path::{math::Point, Builder};
use palette::{
    encoding::{srgb::Srgb, TransferFn},
    Component,
};
use rayon::prelude::*;
use std::{path::PathBuf, rc::Rc, time::Duration};

pub type V2 = Point;

type Result<T> = std::result::Result<T, Error>;

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    /// Seed for the rng.
    #[structopt(short = "e", long = "seed", default_value = "0")]
    pub seed: u64,

    /// Width of view pane.
    #[structopt(short = "w", long = "width", default_value = "512")]
    pub width: u32,

    /// Height of view pane.
    #[structopt(short = "h", long = "height", default_value = "650")]
    pub height: u32,

    /// Scale of view pane.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f32,

    /// Frame range to render from the generate scene.
    #[structopt(short = "f", long = "frames", default_value = "1")]
    pub frames: usize,

    /// The number of frames (to try) to render per second.
    #[structopt(short = "r", long = "frames_per_second", default_value = "24")]
    pub framerate: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,
}

/// The world in which the painting takes place.
#[derive(Debug, Copy, Clone)]
pub struct World {
    /// The RNG seed this painting was given.
    pub seed: u64,
    /// The width in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If width is 500 but scale is 10, the painting
    /// will have a coordinate space width of 500, but the final output will have a width of 5000 pixels.
    pub width: f32,
    /// The height in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If height is 500 but scale is 10, the painting
    /// will have a coordinate space height of 500, but the final output will have a height of 5000 pixels.
    pub height: f32,
    /// The scale of the output.
    ///
    /// The final output space is (width*scale)x(height*scale). This value is useful for painting
    /// and doing work at one quickly rendering resolution, and later exporting at a much higher
    /// resolution while preserving the composition exactly.
    ///
    /// This value may be needed when writing shaders or using other raster graphics, to adjust them
    /// for the real output size. Vector painting such as with paths should not need to consider this.
    pub scale: f32,
    /// The total number of frames in this painting.
    pub frames: usize,
}

impl From<&Options> for World {
    fn from(options: &Options) -> Self {
        Self {
            seed: options.seed,
            width: options.width as f32,
            height: options.height as f32,
            scale: options.scale,
            frames: options.frames,
        }
    }
}

/// The context of the current render frame.
#[derive(Debug, Copy, Clone)]
pub struct FrameContext {
    /// The current frame in the composition.
    pub frame: usize,
}

impl World {
    pub fn normalize(&self, p: V2) -> V2 { V2::new(p.x / self.width, p.y / self.height) }

    pub fn center(&self) -> V2 { V2::new(self.width / 2.0, self.height / 2.0) }
}

impl Paint for World {
    fn paint(&self, comp: &mut Canvas) {
        comp.line_to(V2::new(0.0, 0.0));
        comp.line_to(V2::new(self.width, 0.0));
        comp.line_to(V2::new(self.width, self.height));
        comp.line_to(V2::new(0.0, self.height));
        comp.line_to(V2::new(0.0, 0.0));
    }
}

fn save_path_for_frame(mut base_path: PathBuf, seed: u64, frame: usize) -> PathBuf {
    base_path.push(format!("{}", seed));
    std::fs::create_dir_all(&base_path)
        .expect(&format!("To create save directory {:?}", base_path));
    base_path.push(format!("{}.png", frame));
    base_path
}

/// A trait for types which can be represented in a `Canvas`.
pub trait Paint {
    /// Paints self in the composition.
    fn paint(&self, comp: &mut Canvas);
}

/// A handle to the gpu.
pub struct Gpu<'a> {
    gpu: &'a gpu::Gpu,
}

impl<'a> Gpu<'a> {
    pub fn build_shader(&self, glsl: &str) -> Result<ShaderBuilder> {
        Ok(ShaderBuilder {
            gpu_handle: &self,
            program: self.gpu.compile_glsl(glsl)?,
        })
    }
}

pub struct ShaderBuilder<'a, 'b> {
    gpu_handle: &'a Gpu<'b>,
    program: Rc<Program>,
}

impl<'a, 'b> ShaderBuilder<'a, 'b> {
    // TODO: Take uniform trait bound here
    pub fn build(&self) -> Result<Shader> {
        self.gpu_handle
            .gpu
            .build_shader(self.program.clone(), UniformBuffer::default())
    }
}

/// A render gate renders frames.
pub struct Renderer<'a> {
    gpu: &'a gpu::Gpu,
    events_loop: EventsLoop,
    world: World,
    width: u32,
    height: u32,
    wait: Duration,
    save_dir: Option<PathBuf>,
    frames: usize,
}

impl<'a> Renderer<'a> {
    /// Render all of the frames for the composition. This will not return until until all frames of
    /// the composition have been rendered.
    pub fn render_frames(&mut self, mut f: impl FnMut(&FrameContext, &mut Canvas)) -> Result<()> {
        let default_shader = self
            .gpu
            .default_shader(self.width as f32, self.height as f32);
        let buffer = self.gpu.build_texture(self.width, self.height)?;
        for frame in 0..(self.frames) {
            let mut comp = Canvas::new(default_shader.clone());
            comp.set_scale(self.world.scale);
            f(&FrameContext { frame }, &mut comp);

            if let Some(save_dir) = self.save_dir.as_ref() {
                self.gpu.precompose(
                    self.width,
                    self.height,
                    comp.elements.into_iter(),
                    &mut buffer.as_surface(),
                )?;
                let raw: glium::texture::RawImage2d<u8> = self.gpu.read_to_ram(&buffer)?;
                let image: ImageBuffer<Rgba<u8>, Vec<u8>> = ImageBuffer::from_raw(
                    self.width,
                    self.height,
                    raw.data
                        .into_par_iter()
                        .map(|v| v.convert::<f32>())
                        .map(|v: f32| <Srgb as TransferFn>::from_linear(v))
                        .map(|v| v.convert::<u8>())
                        .collect(),
                )
                .unwrap();

                image.save(save_path_for_frame(
                    save_dir.clone(),
                    self.world.seed,
                    frame,
                ))?;
            } else {
                let mut frame = self
                    .gpu
                    .get_frame()
                    .expect("Expected frame for windowed gpu context");
                self.gpu.precompose(
                    self.width,
                    self.height,
                    comp.elements.into_iter(),
                    &mut frame,
                )?;
                frame.finish().expect("Swapping buffers");

                let mut quit = false;
                self.events_loop.poll_events(|event| {
                    use glutin::{DeviceEvent, Event, KeyboardInput, VirtualKeyCode};
                    match event {
                        Event::DeviceEvent {
                            event:
                                DeviceEvent::Key(KeyboardInput {
                                    virtual_keycode: Some(VirtualKeyCode::Escape),
                                    ..
                                }),
                            ..
                        } => {
                            quit = true;
                        }
                        _ => {}
                    }
                });
                if quit {
                    return Ok(());
                }

                std::thread::sleep(self.wait);
            }
        }
        Ok(())
    }
}

/// Run a composition.
pub fn run(
    options: Options,
    mut f: impl FnMut(&Gpu, &World, &mut StdRng, Renderer) -> Result<()>,
) -> Result<()> {
    let world = World::from(&options);

    let (width, height) = (
        options.width as f32 * options.scale,
        options.height as f32 * options.scale,
    );
    let mut rng = StdRng::seed_from_u64(options.seed);

    let (gpu, events_loop) = if options.output.is_some() {
        gpu::Gpu::new()?
    } else {
        gpu::Gpu::with_window(width as u32, height as u32)?
    };

    let gate = Renderer {
        gpu: &gpu,
        events_loop,
        world,
        width: width as u32,
        height: height as u32,
        wait: Duration::from_secs_f64(1. / options.framerate as f64),
        save_dir: options.output,
        frames: options.frames,
    };

    let gpu = Gpu { gpu: &gpu };

    f(&gpu, &world, &mut rng, gate)
}

/// A painting surface.
pub struct Canvas {
    path: Builder,
    shader: Shader,
    color: LinSrgba,
    stroke_thickness: f32,
    scale: f32,
    elements: Vec<Element>,
}

impl Canvas {
    fn new(default_shader: Shader) -> Self {
        Self {
            path: Builder::new(),
            shader: default_shader,
            color: Alpha::<LinSrgb, _>::new(1., 1., 1., 1.),
            scale: 1.,
            stroke_thickness: 1.,
            elements: vec![],
        }
    }

    /// Sets the scale of all following path operations.
    fn set_scale(&mut self, scale: f32) { self.scale = scale; }

    /// Paints an element.
    pub fn paint(&mut self, element: impl Paint) { element.paint(self); }

    /// Sets the current color.
    pub fn set_color(&mut self, color: impl IntoColor) {
        self.color = Alpha::from(color.into_rgb());
    }

    /// Sets the current color.
    pub fn set_color_with_alpha(&mut self, color_alpha: Alpha<impl IntoColor, f32>) {
        self.color = Alpha {
            color: color_alpha.color.into_rgb(),
            alpha: color_alpha.alpha,
        };
    }

    /// Stats a new path at the given point.
    pub fn move_to(&mut self, dest: V2) {
        self.path = Builder::new();
        self.path.move_to(dest * self.scale);
    }

    /// Adds a line to the current path which ends at the given point.
    pub fn line_to(&mut self, dest: V2) { self.path.line_to(dest * self.scale); }

    /// Adds a quadratic bezier curve to the current path with the given control and end points.
    pub fn quadratic_to(&mut self, ctrl: V2, end: V2) {
        self.path
            .quadratic_bezier_to(ctrl * self.scale, end * self.scale);
    }

    /// Adds a cubic bezier curve to the current path with the given control and end points.
    pub fn cubic_to(&mut self, ctrl0: V2, ctrl1: V2, end: V2) {
        self.path
            .cubic_bezier_to(ctrl0 * self.scale, ctrl1 * self.scale, end * self.scale);
    }

    /// Closes the current path.
    pub fn close(&mut self) { self.path.close() }

    /// Sets the thickness of lines drawn with the `stroke()`.
    pub fn set_stroke_thickness(&mut self, stroke_thickness: f32) {
        self.stroke_thickness = stroke_thickness;
    }

    /// Paints the current path by filling the region inside the path.
    pub fn fill(&mut self) { self.push_element(Method::Fill); }

    /// Paints the current path by stroking the path.
    pub fn stroke(&mut self) { self.push_element(Method::Stroke(self.stroke_thickness)); }

    /// Sets the current shader used to shade rastered paths.
    ///
    /// Changing shaders requires making a new draw call to the GPU and tearing down some state.
    /// Changing shaders 0-10 times per frame is likely to be fast enough. Changing shaders 500
    /// times per frame will be slow.
    pub fn set_shader(&mut self, shader: Shader) { self.shader = shader; }

    fn push_element(&mut self, raster_method: Method) {
        let mut path = Builder::new();
        std::mem::swap(&mut self.path, &mut path);

        self.elements.push(Element {
            path,
            color: self.color,
            shader: self.shader.clone(),
            raster_method,
        });
    }
}
