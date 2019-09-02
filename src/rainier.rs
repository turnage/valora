//! Tools for composing generative fine art.

use crate::amicola::*;

pub use crate::amicola::{Error, Polygon, Shader, V2, V4};
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};

use std::convert::TryFrom;
use std::path::PathBuf;
use structopt::StructOpt;

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
    #[structopt(short = "h", long = "height", default_value = "512")]
    pub height: u32,

    /// Scale of view pane.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f64,

    /// Frame range to render from the generate scene.
    #[structopt(short = "f", long = "frames", default_value = "1")]
    pub frames: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: PathBuf,
}

#[derive(Debug)]
pub struct Context<S> {
    pub width: f64,
    pub height: f64,
    pub seed: u64,
    pub frames: usize,
    pub frame: usize,
    pub state: S,
}

impl<S> From<(Options, S)> for Context<S> {
    fn from((options, state): (Options, S)) -> Self {
        Self {
            seed: options.seed,
            width: options.width as f64,
            height: options.height as f64,
            frames: options.frames,
            frame: 0,
            state,
        }
    }
}

impl<S> Context<S> {
    pub fn normalize(&self, p: V2) -> V2 {
        V2::new(p.x / self.width, p.y / self.height)
    }

    pub fn center(&self) -> V2 {
        V2::new(self.width / 2.0, self.height / 2.0)
    }

    pub fn full_frame(&self) -> Polygon {
        Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(self.width, 0.0),
            V2::new(self.width, self.height),
            V2::new(0.0, self.height),
        ])
        .unwrap()
    }
}

pub trait Generate<S> {
    type Output;

    fn generate(&self, ctx: &Context<S>, rng: &mut StdRng) -> Self::Output;
}

impl<S, T, F: Fn(&Context<S>, &mut StdRng) -> T> Generate<S> for F {
    type Output = T;

    fn generate(&self, ctx: &Context<S>, rng: &mut StdRng) -> Self::Output {
        (self)(ctx, rng)
    }
}

pub trait Render {
    fn render(&self, comp: &mut Composition);
}

pub trait Composer<S> {
    fn init(rng: &mut StdRng) -> S;
    fn draw(&mut self, ctx: &Context<S>, rng: &mut StdRng, comp: &mut Composition) -> S;
}

pub fn run<S, C: Composer<S>>(mut composer: C) {
    let options = Options::from_args();

    let mut rng = StdRng::seed_from_u64(options.seed);
    let state = <C as Composer<S>>::init(&mut rng);
    let mut ctx = Context::from((options.clone(), state));
    let mut comp = Composition::with_dimensions(options.width, options.height, options.scale);

    for frame in 0..(options.frames) {
        ctx.frame = frame;
        println!("Running frame {:?}", frame);
        let next_state = composer.draw(&ctx, &mut rng, &mut comp);
        ctx.state = next_state;

        let mut save_path = options.output.clone();
        save_path.push(format!("{}", ctx.seed));
        std::fs::create_dir_all(&save_path)
            .expect(&format!("To create save directory {:?}", save_path));
        save_path.push(format!("{}.png", ctx.frame));
        comp.render().save(save_path).expect("To save output.");
    }
}

pub struct Composition {
    surface: Surface,
    current_path: Vec<V2>,
    current_shader: Shader,
    scale: f64,
}

impl Composition {
    fn with_dimensions(width: u32, height: u32, scale: f64) -> Self {
        let width = width as f64 * scale;
        let height = height as f64 * scale;
        Self {
            surface: Surface::with_dimensions(width as u32, height as u32),
            current_path: vec![],
            current_shader: Shader::Solid(V4::new(1.0, 1.0, 1.0, 1.0)),
            scale,
        }
    }

    pub fn move_to(&mut self, dest: V2) {
        self.current_path = vec![dest * self.scale];
    }

    pub fn line_to(&mut self, dest: V2) {
        self.current_path.push(dest * self.scale);
    }

    pub fn set_shader(&mut self, shader: Shader) {
        self.current_shader = shader;
    }

    pub fn fill(&mut self) {
        let mut path = vec![];
        std::mem::swap(&mut self.current_path, &mut path);
        raster(
            &mut self.surface,
            Element {
                path,
                shader: self.current_shader.clone(),
                raster_method: RasterMethod::Fill,
            },
        );
    }

    pub fn render(&self) -> FinalBuffer {
        self.surface.clone().into()
    }
}
