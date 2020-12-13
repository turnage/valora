#![cfg_attr(feature = "ci", deny(warnings))]

//! A brush for generative fine art.

mod noise_traits;
mod raster;

pub mod attributes;
pub mod canvas;
pub mod forms;
pub mod paint;
pub mod path;
pub mod transforms;
pub mod uniforms;

/// Exhuastive set of imports for painting.
pub mod prelude {
    pub use self::{
        attributes::*, canvas::*, forms::*, paint::*, path::*, transforms::*, uniforms::*,
    };
    pub use super::*;
    pub use euclid;
    pub use noise::{self, *};
    pub use noise_traits::*;
    pub use rayon::{self, prelude::*};

    pub use palette::{
        self, encoding::Srgb, Alpha, Blend, ComponentWise, Hue, IntoColor, LinSrgb, LinSrgba,
        Saturate, *,
    };
    pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
    pub use structopt::StructOpt;

    pub use std::f32::consts::PI;
}

use self::{prelude::*, raster::Method};
use amicola::SampleDepth;
use anyhow::Error;
use euclid::{Point3D, Size2D, UnknownUnit, Vector2D, Vector3D};
use itertools::Itertools;
use lyon_path::math::Point;
use pirouette::*;
use rand::random;
use std::io::ErrorKind;
use std::rc::Rc;
use std::{
    path::{Path, PathBuf},
    time::Duration,
};
use wgpu::*;

/// A two dimensional point.
pub type P2 = Point;

/// A three dimensional point.
pub type P3 = Point3D<f32, UnknownUnit>;

/// A two dimensional vector.
pub type V2 = Vector2D<f32, UnknownUnit>;

/// A three dimensional vector.
pub type V3 = Vector3D<f32, UnknownUnit>;

/// A two dimensional size.
pub type S2 = Size2D<f32, UnknownUnit>;

/// An angle.
pub type Angle = euclid::Angle<f32>;

/// A compiled GLSL program.
pub type Program = glium::program::Program;

/// A value or an error.
pub type Result<T> = std::result::Result<T, Error>;

/// The context of the current render frame.
#[derive(Debug, Clone, Copy)]
pub struct Context {
    /// The world in which painting takes place.
    pub world: World,
    /// The current frame in the composition.
    pub frame: usize,
    /// The elapsed time in the composition.
    pub time: Duration,
    /// The root random seed.
    pub seed: u64,
}

/// Command line options for a painting run.
///
/// Construct with `Options::from_args()` to run the CLI.
#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    #[structopt(flatten)]
    pub world: World,

    /// Brainstorm mode will repeat the render from frame 0 with a new random
    /// seed. The value prescribes the number of seeds to try.
    #[structopt(short = "b", long = "brainstorm", default_value = "1")]
    pub brainstorm: usize,

    /// The number of frames to delay saving to file. For example, if delay=100,
    /// 100 frames will be rendered silently and then the 101st and those after it
    /// will be saved to file.
    #[structopt(short = "d", long = "delay", default_value = "0")]
    pub delay: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// Sample depth is the number of times of subdivide and sample pixels at
    /// the boundaries of paths. This controls the antialiasing quality of the
    /// rasterizer. It recommended to leave this at 1 for quick iteration and
    /// turn it up for final output. The highest value is "super64".
    #[structopt(subcommand)]
    pub sample_depth: Option<amicola::SampleDepth>,
}

/// The world in which the painting takes place.
#[derive(StructOpt, Debug, Copy, Clone)]
#[structopt(name = "world")]
pub struct World {
    /// The RNG seed for this painting.
    #[structopt(short = "e", long = "seed", default_value = "0")]
    pub seed: u64,

    /// The width in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If width is 500 but scale is 10, the painting
    /// will have a coordinate space width of 500, but the final output will have a width of 5000 pixels.
    #[structopt(short = "w", long = "width", default_value = "512")]
    pub width: f32,
    /// The height in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If height is 500 but scale is 10, the painting
    /// will have a coordinate space height of 500, but the final output will have a height of 5000 pixels.
    #[structopt(short = "h", long = "height", default_value = "650")]
    pub height: f32,

    /// The scale of the output.
    ///
    /// The final output space is (width*scale)x(height*scale). This value is useful for painting
    /// and doing work at one quickly rendering resolution, and later exporting at a much higher
    /// resolution while preserving the composition exactly.
    ///
    /// This value may be needed when writing shaders or using other raster graphics, to adjust them
    /// for the real output size. Vector painting such as with paths should not need to consider this.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f32,

    /// The total number of frames in this painting.
    #[structopt(short = "f", long = "frames")]
    pub frames: Option<usize>,

    /// The number of frames (to try) to render per second.
    #[structopt(short = "r", long = "frames_per_second", default_value = "24")]
    pub framerate: usize,
}

impl World {
    /// Normalizes coordinates into the range [0, 1] by dividing them by the coordinate space dimensions.
    pub fn normalize(&self, p: P2) -> P2 {
        P2::new(p.x / self.width, p.y / self.height)
    }

    /// Returns the center of the coordinate space.
    pub fn center(&self) -> P2 {
        P2::new(self.width / 2.0, self.height / 2.0)
    }

    /// Returns a `Rect` representing the entire frame.
    pub fn rect(&self) -> Rect {
        Rect {
            bottom_left: P2::new(0., 0.),
            width: self.width,
            height: self.height,
        }
    }
}

/// Draws a rectangle path covering the entire canvas.
impl Paint for World {
    fn paint(&self, comp: &mut Canvas) {
        comp.line_to(P2::new(0.0, 0.0));
        comp.line_to(P2::new(self.width, 0.0));
        comp.line_to(P2::new(self.width, self.height));
        comp.line_to(P2::new(0.0, self.height));
        comp.line_to(P2::new(0.0, 0.0));
    }
}

/// A rasterable element in a composition.
pub(crate) struct Element {
    pub path: lyon_path::Builder,
    /// Whether the path is closed.
    pub closed: bool,
    pub color: LinSrgba,
    pub raster_method: Method,
    pub shader: Option<Rc<ShaderModule>>,
}

/// A trait for types which paint canvases.
pub trait Artist: Sized {
    /// Constructs the artist.
    ///
    /// This would be a place to compile any GLSL or construct any expensive
    /// resources needed across the whole composition.
    fn setup(world: World, rng: &mut StdRng) -> Result<Self>;

    /// Paints a single frame.
    fn paint(&mut self, ctx: Context, canvas: &mut Canvas);
}

/// Run an artist defined by raw functions.
///
/// Takes a function that produces the function that should paint each frame.
pub fn run_fn<F>(options: Options, f: impl Fn(World, &mut StdRng) -> Result<F>) -> Result<()>
where
    F: FnMut(Context, &mut StdRng, &mut Canvas),
{
    let (output_width, output_height) = (
        (options.world.width as f32 * options.world.scale) as u32,
        (options.world.height as f32 * options.world.scale) as u32,
    );

    let instance = Instance::new(BackendBit::PRIMARY);
    let adapter = instance
        .enumerate_adapters(BackendBit::PRIMARY)
        .next()
        .expect("No supported wgpu backend found");
    println!("Adapter: {:?}", adapter.get_info());
    let (device, queue) = futures::executor::block_on(adapter.request_device(
        &DeviceDescriptor {
            features: Features::empty(),
            limits: Limits::default(),
            shader_validation: true,
        },
        /*trace_path=*/ Some(&Path::new("wgpu_trace.txt")),
    ))?;

    if let Some(path) = options.output.as_ref() {
        prepare_output_dir(path)?
    }

    let scope = Scope {
        scale: options.world.scale,
        offset: [0., 0.],
        dimensions: [output_width, output_height],
    };
    let framerate = options.world.framerate;
    let time = move |frame| Duration::from_secs_f32(frame as f32 / framerate as f32);

    let render_target = device.create_buffer(&BufferDescriptor {
        label: None,
        size: (options.world.width * options.world.height) as u64 * 16,
        usage: BufferUsage::STORAGE | BufferUsage::COPY_SRC,
        mapped_at_creation: false,
    });

    let mut renderer = Renderer::new(&device);
    let job = renderer.job(scope, &device);

    let mut seeds = std::iter::successors(Some(options.world.seed), |_| Some(random()));
    for _ in 0..(options.brainstorm) {
        let mut ctx = Context {
            world: options.world,
            frame: 0,
            time: time(0),
            seed: seeds.next().unwrap(),
        };
        let mut rng = StdRng::seed_from_u64(ctx.seed);
        let mut runner = f(options.world, &mut rng)?;
        for i in 0..(options.world.frames.unwrap_or(100)) {
            ctx.frame = i;
            println!("Rendering frame... {:?}", i);
            let mut canvas = Canvas::new(options.world.scale);
            runner(ctx, &mut rng, &mut canvas);
            let elements = canvas.elements().into_iter();
            for (_, batch) in &elements.group_by(|e: &Element| {
                e.shader
                    .as_ref()
                    .map(Rc::as_ptr)
                    .map(|p| p as u64)
                    .unwrap_or(0)
            }) {
                let mut shader: Option<Rc<ShaderModule>> = None;
                let lines: Vec<Vertex> = batch
                    .flat_map(|element| {
                        if let Some(s) = element.shader {
                            shader.replace(s);
                        }
                        raster::raster_path(
                            element.path,
                            element.closed,
                            element.raster_method,
                            element.color,
                            options.sample_depth.unwrap_or(SampleDepth::Single),
                        )
                    })
                    .collect();
                let commands = renderer.render(&job, lines.as_slice(), &device);

                queue.submit(std::iter::once(commands));
                device.poll(Maintain::Wait);

                match options.output.as_ref() {
                    Some(directory_name) => {
                        renderer.export(&job, &device, &queue, |image_buffer| {
                            let mut output_path = PathBuf::new();
                            output_path.push(directory_name);
                            output_path.push(format!("seed_{}-frame_{}.png", ctx.seed, ctx.frame));
                            Ok(image_buffer.save(output_path)?)
                        })?;
                    }
                    None => {}
                }
            }
        }
    }

    Ok(())
}

fn prepare_output_dir(path: impl AsRef<Path>) -> Result<()> {
    match std::fs::create_dir_all(path) {
        Err(e) if e.kind() != ErrorKind::AlreadyExists => Err(e)?,
        _ => Ok(()),
    }
}

/// Run an artist.
pub fn run<A: Artist>(options: Options) -> Result<()> {
    run_fn(options, |world, rng| {
        let mut artist = A::setup(world, rng)?;
        Ok(move |ctx: Context, rng: &mut StdRng, canvas: &mut Canvas| artist.paint(ctx, canvas))
    })
}
