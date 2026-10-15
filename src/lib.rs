#![cfg_attr(feature = "ci", deny(warnings))]

//! A brush for generative fine art.

mod gpu;
mod noise_traits;
mod raster;
mod render;
mod server;

pub mod attributes;
pub mod canvas;
pub mod forms;
pub mod paint;
pub mod path;
pub mod shaders;
pub mod transforms;
pub mod uniforms;

/// Exhuastive set of imports for painting.
pub mod prelude {
    pub use self::{
        attributes::*, canvas::*, forms::*, paint::*, path::*, transforms::*, uniforms::*,
    };
    pub use super::*;
    pub use ::euclid;
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

pub use self::{
    gpu::{Gpu, Shader},
    render::Context,
    shaders::ShaderProgram,
};

use self::{gpu::*, prelude::*, raster::Method};
use anyhow::{Context as _, Error};
use euclid::{Point3D, Size2D, UnknownUnit, Vector2D, Vector3D};
use lyon_path::math::Point;
use render::*;
use server::Server;
use std::net::SocketAddr;
use std::path::PathBuf;

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

/// A value or an error.
pub type Result<T> = std::result::Result<T, Error>;

/// Command line options for a painting run.
///
/// Construct with `Options::from_args()` to run the CLI.
#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    #[structopt(flatten)]
    pub world: World,

    /// In brainstorm mode:
    ///
    ///   * When rendering a limited number of frames to screen, the preview will not close.
    ///
    ///   * When rendering to file, every frame will be rendered with a different seed.
    #[structopt(short = "b", long = "brainstorm")]
    pub brainstorm: bool,

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

    /// The port to serve the composition's page on, when not rendering to
    /// files.
    #[structopt(long = "port", default_value = "6161")]
    pub port: u16,
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

    /// The width and height of rendered output in pixels.
    pub fn output_size(&self) -> (u32, u32) {
        (
            (self.width * self.scale) as u32,
            (self.height * self.scale) as u32,
        )
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

/// A trait for types which paint canvases.
pub trait Artist: Sized {
    /// Constructs the artist.
    ///
    /// This would be a place to compile any shaders or construct any expensive
    /// resources needed across the whole composition.
    fn setup(gpu: Gpu, world: World, rng: &mut StdRng) -> Result<Self>;

    /// Paints a single frame.
    fn paint(&mut self, ctx: Context, canvas: &mut Canvas);
}

/// Run an artist defined by raw functions.
///
/// Takes a function that produces the function that should paint each frame.
///
/// Frames are saved to files if an output path is given. Otherwise, they are
/// shown on a web page served at the port in `options`.
pub fn run_fn<F>(options: Options, f: impl Fn(Gpu, World, &mut StdRng) -> Result<F>) -> Result<()>
where
    F: FnMut(Context, &mut Canvas),
{
    let number_width = options
        .world
        .frames
        .unwrap_or(0)
        .to_string()
        .chars()
        .count();

    let gpu = Gpu::new()?;
    let (output_width, output_height) = options.world.output_size();
    gpu.check_target_size(output_width, output_height)?;

    let output = match options.output.clone() {
        Some(base_path) => {
            std::fs::create_dir_all(&base_path)
                .with_context(|| format!("creating save directory {}", base_path.display()))?;
            Output::File {
                base_path,
                number_width,
            }
        }
        None => Output::Serve {
            server: Server::start(SocketAddr::from(([127, 0, 0, 1], options.port)))?,
            paused: false,
        },
    };

    let mut renderer = Renderer {
        output,
        gpu: gpu.clone(),
        options: options.clone(),
    };

    let mut world = options.world;
    for run in 0.. {
        let mut rng = StdRng::seed_from_u64(world.seed);
        let mut paint_fn = f(gpu.clone(), world, &mut rng)?;
        match renderer.render_frames(world, &mut rng, |ctx, canvas| paint_fn(ctx, canvas))? {
            Next::Rebuild(new_world) => world = new_world,
            Next::Done
                if options.brainstorm
                    && options.output.is_some()
                    && run < options.world.frames.unwrap_or(usize::MAX) =>
            {
                world.seed += 1;
            }
            Next::Done => break,
        }
    }

    Ok(())
}

/// Run an artist.
pub fn run<A: Artist>(options: Options) -> Result<()> {
    run_fn(options, |gpu, world, rng| {
        let mut artist = A::setup(gpu, world, rng)?;
        Ok(move |ctx: Context, canvas: &mut Canvas| artist.paint(ctx, canvas))
    })
}
