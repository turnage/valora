//! Tools for composing generative fine art.

mod gpu;
mod raster;

pub use self::gpu::{Shader, UniformBuffer};
pub use glium::program::Program;
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
pub use structopt::StructOpt;

use self::{gpu::*, raster::Method};
use failure::Error;
use image::{ImageBuffer, Rgba};
use lyon_path::{math::Point, Builder};
use nalgebra::{base::*, Matrix};
use std::{path::PathBuf, rc::Rc};

pub type V2 = Matrix<f32, U2, U1, ArrayStorage<f32, U2, U1>>;
pub type V3 = Matrix<f32, U3, U1, ArrayStorage<f32, U3, U1>>;
pub type V4 = Matrix<f32, U4, U1, ArrayStorage<f32, U4, U1>>;

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

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: PathBuf,
}

#[derive(Debug, Copy, Clone)]
pub struct World {
    pub seed: u64,
    pub width: f32,
    pub height: f32,
    pub scale: f32,
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

#[derive(Debug, Copy, Clone)]
pub struct FrameContext {
    /// The current frame in the composition.
    pub frame: usize,
}

impl World {
    pub fn normalize(&self, p: V2) -> V2 { V2::new(p.x / self.width, p.y / self.height) }

    pub fn center(&self) -> V2 { V2::new(self.width / 2.0, self.height / 2.0) }
}

impl Draw for World {
    fn draw(&self, comp: &mut Composition) {
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

pub trait Draw {
    fn draw(&self, comp: &mut Composition);
}

pub struct GpuHandle<'a> {
    gpu: &'a Gpu,
}

impl<'a> GpuHandle<'a> {
    pub fn build_shader(&self, glsl: &str) -> Result<ShaderBuilder> {
        Ok(ShaderBuilder {
            gpu_handle: &self,
            program: self.gpu.compile_glsl(glsl)?,
        })
    }
}

pub struct ShaderBuilder<'a, 'b> {
    gpu_handle: &'a GpuHandle<'b>,
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

pub struct RenderGate<'a> {
    gpu: &'a Gpu,
    world: World,
    width: u32,
    height: u32,
    save_dir: PathBuf,
    frames: usize,
}

impl<'a> RenderGate<'a> {
    pub fn render_frames(
        &mut self,
        mut f: impl FnMut(&FrameContext, &mut Composition),
    ) -> Result<()> {
        let default_shader = self
            .gpu
            .default_shader(self.width as f32, self.height as f32);
        for frame in 0..(self.frames) {
            let mut comp = Composition::new(default_shader.clone());
            comp.set_scale(self.world.scale);
            f(&FrameContext { frame }, &mut comp);

            println!("Rendering to texture");
            let buffer = self
                .gpu
                .precompose(self.width, self.height, comp.elements.into_iter())?;
            println!("Reading to ram...");

            let raw: glium::texture::RawImage2d<u8> = self.gpu.read_to_ram(buffer.as_ref())?;
            println!("encoding as image...");
            let image: ImageBuffer<Rgba<u8>, Vec<u8>> =
                ImageBuffer::from_raw(self.width, self.height, raw.data.into_owned()).unwrap();
            println!("saving image to disk...");

            image.save(save_path_for_frame(
                self.save_dir.clone(),
                self.world.seed,
                frame,
            ))?;
        }
        Ok(())
    }
}

pub fn run(
    options: Options,
    mut f: impl FnMut(&GpuHandle, &World, &mut StdRng, RenderGate) -> Result<()>,
) -> Result<()> {
    let world = World::from(&options);

    let (width, height) = (
        options.width as f32 * options.scale,
        options.height as f32 * options.scale,
    );
    let mut rng = StdRng::seed_from_u64(options.seed);

    let gpu = Gpu::new()?;
    let gate = RenderGate {
        gpu: &gpu,
        world,
        width: width as u32,
        height: height as u32,
        save_dir: options.output,
        frames: options.frames,
    };

    let gpu_handle = GpuHandle { gpu: &gpu };

    f(&gpu_handle, &world, &mut rng, gate)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SampleDepth {
    Single,
    Super4,
    Super8,
    Super16,
    Super32,
    Super64,
}

impl Into<u64> for SampleDepth {
    fn into(self) -> u64 {
        match self {
            SampleDepth::Single => 1,
            SampleDepth::Super4 => 4,
            SampleDepth::Super8 => 8,
            SampleDepth::Super16 => 16,
            SampleDepth::Super32 => 32,
            SampleDepth::Super64 => 64,
        }
    }
}

pub struct Composition {
    path: Builder,
    shader: Shader,
    color: V4,
    stroke_thickness: f32,
    scale: f32,
    sample_depth: SampleDepth,
    elements: Vec<Element>,
}

impl Composition {
    fn new(default_shader: Shader) -> Self {
        Self {
            path: Builder::new(),
            shader: default_shader,
            color: V4::new(1.0, 1.0, 1.0, 1.0),
            scale: 1.,
            stroke_thickness: 1.,
            sample_depth: SampleDepth::Single,
            elements: vec![],
        }
    }

    pub fn draw(&mut self, element: impl Draw) { element.draw(self); }

    pub fn set_sample_depth(&mut self, sample_depth: SampleDepth) {
        self.sample_depth = sample_depth;
    }

    pub fn set_scale(&mut self, scale: f32) { self.scale = scale; }

    pub fn move_to(&mut self, dest: V2) {
        self.path = Builder::new();
        self.path
            .move_to(Point::new(dest.x * self.scale, dest.y * self.scale));
    }

    pub fn line_to(&mut self, dest: V2) {
        self.path
            .line_to(Point::new(dest.x * self.scale, dest.y * self.scale));
    }

    pub fn quadratic_to(&mut self, ctrl: V2, end: V2) {
        self.path.quadratic_bezier_to(
            Point::new(ctrl.x * self.scale, ctrl.y * self.scale),
            Point::new(end.x * self.scale, end.y * self.scale),
        );
    }

    pub fn cubic_to(&mut self, ctrl0: V2, ctrl1: V2, end: V2) {
        self.path.cubic_bezier_to(
            Point::new(ctrl0.x * self.scale, ctrl0.y * self.scale),
            Point::new(ctrl1.x * self.scale, ctrl1.y * self.scale),
            Point::new(end.x * self.scale, end.y * self.scale),
        );
    }

    pub fn set_color(&mut self, color: V4) { self.color = color; }

    pub fn set_shader(&mut self, shader: Shader) { self.shader = shader; }

    pub fn set_stroke_thickness(&mut self, stroke_thickness: f32) {
        self.stroke_thickness = stroke_thickness;
    }

    pub fn fill(&mut self) { self.push_element(Method::Fill); }

    pub fn stroke(&mut self) { self.push_element(Method::Stroke(self.stroke_thickness)); }

    fn push_element(&mut self, raster_method: Method) {
        let mut path = Builder::new();
        std::mem::swap(&mut self.path, &mut path);

        let sample_depth = self.sample_depth;
        self.elements.push(Element {
            path,
            color: self.color,
            shader: self.shader.clone(),
            raster_method,
            sample_depth,
        });
    }
}
