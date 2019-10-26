//! Tools for composing generative fine art.

use crate::amicola::*;

pub use crate::amicola::{Error, Polygon, Shader, V2, V4};
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};

use std::convert::TryFrom;
use std::path::PathBuf;
use std::rc::Rc;
use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    /// Seed for the rng.
    #[structopt(short = "e", long = "seed", default_value = "0")]
    pub seed: u64,

    /// Width of view pane.
    #[structopt(short = "w", long = "width", default_value = "1024")]
    pub width: u32,

    /// Height of view pane.
    #[structopt(short = "h", long = "height", default_value = "1308")]
    pub height: u32,

    /// Scale of view pane.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f64,

    /// Frame range to render from the generate scene.
    #[structopt(short = "f", long = "frames", default_value = "1")]
    pub frames: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,
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
    fn render(&self, comp: &mut Sketch);
}

pub trait Composer<S> {
    fn init(rng: &mut StdRng) -> S;
    fn draw(&mut self, ctx: &Context<S>, rng: &mut StdRng, comp: &mut Sketch) -> S;
}

pub trait Sketch {
    fn move_to(&mut self, dest: V2);

    fn line_to(&mut self, dest: V2);

    fn set_shader(&mut self, shader: Shader);

    fn fill(&mut self);
}

pub fn run<S, C: Composer<S>>(mut composer: C) {
    let options = Options::from_args();

    let mut rng = StdRng::seed_from_u64(options.seed);
    let state = <C as Composer<S>>::init(&mut rng);
    let mut ctx = Context::from((options.clone(), state));

    if let Some(output) = options.output.as_ref() {
        println!("unsupported");
    } else {
        use luminance_glfw::{Key, WindowEvent};

        let mut comp = Rainier::new(
            GpuTarget::with_dimensions(options.width as u32, options.height as u32),
            options.scale,
        );
        let mut frame = 0;

        'outer: loop {
            ctx.frame = frame % options.frames;
            let next_state = composer.draw(&ctx, &mut rng, &mut comp);
            ctx.state = next_state;
            /*comp.target.show();

            let events = comp.target.events();
            for event in events {
                match event {
                    WindowEvent::Close | WindowEvent::Key(Key::Escape, _, _, _) => break 'outer,
                    _ => {
                        println!("Event: {:?}", event);
                    }
                }
            }*/

            std::thread::sleep_ms(1000 / 60);
        }
    }
}

pub struct Rainier<T> {
    target: T,
    current_path: Vec<V2>,
    current_shader: Shader,
    scale: f64,
}

impl<T> Rainier<T> {
    fn new(target: T, scale: f64) -> Self {
        Self {
            target,
            current_path: vec![],
            current_shader: Shader::Solid(V4::new(1.0, 1.0, 1.0, 1.0)),
            scale,
        }
    }
}

impl<T: RasterTarget> Sketch for Rainier<T> {
    fn move_to(&mut self, dest: V2) {
        self.current_path = vec![dest * self.scale];
    }

    fn line_to(&mut self, dest: V2) {
        self.current_path.push(dest * self.scale);
    }

    fn set_shader(&mut self, shader: Shader) {
        self.current_shader = shader;
    }

    fn fill(&mut self) {
        let mut path = vec![];
        std::mem::swap(&mut self.current_path, &mut path);
        self.target.raster(Element {
            path,
            shader: self.current_shader.clone(),
            raster_method: RasterMethod::Fill,
        });
    }
}
