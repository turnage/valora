//! Canvas rendering.

use crate::{canvas::*, gpu::*, server::Server, Options, Result, World};
use anyhow::ensure;
use rand::{random, rngs::StdRng};
use std::convert::{TryFrom, TryInto};
use std::{
    path::PathBuf,
    time::{Duration, Instant},
};
use valora_web::protocol::{self, Command, FrameHeader};

/// The context of the current render frame.
#[derive(Debug)]
pub struct Context<'a> {
    /// A random number generator. This is shared between frames.
    ///
    /// To branch the rng, keep a clone.
    pub rng: &'a mut StdRng,
    /// The world in which painting takes place.
    pub world: World,
    /// The current frame in the composition.
    pub frame: usize,
    /// The elapsed time in the composition.
    pub time: Duration,
}

/// What to do once a composition stops rendering.
pub(crate) enum Next {
    /// Paint the composition again from the start in the given world.
    Rebuild(World),
    /// The composition is finished.
    Done,
}

/// Where rendered frames go.
pub(crate) enum Output {
    /// Frames are saved as PNG files.
    File {
        base_path: PathBuf,
        number_width: usize,
    },
    /// Frames are sent to pages served on the web.
    Serve { server: Server, paused: bool },
}

fn output_path(base_path: PathBuf, number_width: usize, frame_number: usize, seed: u64) -> PathBuf {
    let mut base_path = base_path.clone();
    base_path.push(format!(
        "{}_{number:>0width$}.png",
        seed,
        number = frame_number,
        width = number_width
    ));
    base_path
}

/// A render gate renders frames.
pub(crate) struct Renderer {
    pub output: Output,
    pub gpu: Gpu,
    pub options: Options,
}

impl Renderer {
    /// Render all of the frames for the composition. When serving, this does
    /// not return until a page asks for the composition to be rebuilt.
    pub fn render_frames(
        &mut self,
        world: World,
        rng: &mut StdRng,
        mut f: impl FnMut(Context, &mut Canvas),
    ) -> Result<Next> {
        let seed = world.seed;
        let (output_width, output_height) = world.output_size();
        let frame_duration = Duration::from_secs_f64(1. / world.framerate as f64);
        // Brainstorming to files renders one frame of each seed.
        let frames = match (&self.output, self.options.brainstorm) {
            (Output::File { .. }, true) => Some(1),
            _ => world.frames,
        };
        let end_frame = frames.map(|f| f + self.options.delay);
        let target = self.gpu.create_target(output_width, output_height);
        let mut last_frame = None;

        for frame in (0..).take_while(|frame| end_frame.is_none_or(|end| *frame <= end)) {
            let start = Instant::now();
            let time = Duration::from_secs_f32(frame as f32 / world.framerate as f32);
            let mut canvas = Canvas::new(self.gpu.default_shader(), world.scale);
            f(
                Context {
                    rng,
                    world: World { frames, ..world },
                    frame,
                    time,
                },
                &mut canvas,
            );

            self.gpu.render(
                &target,
                canvas.elements(),
                FrameUniforms {
                    scale: world.scale,
                    time,
                    sample_depth: self
                        .options
                        .sample_depth
                        .unwrap_or(amicola::SampleDepth::Single),
                },
            )?;

            match &mut self.output {
                Output::File {
                    base_path,
                    number_width,
                } => {
                    if frame > self.options.delay {
                        image::save_buffer(
                            output_path(base_path.clone(), *number_width, frame, seed),
                            &self.gpu.read(&target)?,
                            output_width,
                            output_height,
                            image::ExtendedColorType::Rgba8,
                        )?;
                    }
                }
                Output::Serve { server, paused } => {
                    let mut header = FrameHeader {
                        width: output_width,
                        height: output_height,
                        frame: frame as u64,
                        render_micros: start.elapsed().as_micros() as u64,
                        paused: *paused,
                        max_dimension: self.gpu.max_dimension(),
                        world: world.into(),
                    };
                    let pixels = self.gpu.read(&target)?;
                    server.publish(&header, &pixels);

                    let wait = Wait::WhilePaused;
                    if let Some(next) =
                        serve_commands(server, paused, wait, world, &self.gpu, &mut header, &pixels)
                    {
                        return Ok(next);
                    }
                    last_frame = Some((header, pixels));

                    std::thread::sleep(frame_duration.saturating_sub(start.elapsed()));
                }
            }
        }

        match &mut self.output {
            Output::File { .. } => Ok(Next::Done),
            Output::Serve { .. } if self.options.brainstorm => Ok(Next::Rebuild(world)),
            // Keep showing the last frame until a page asks for more.
            Output::Serve { server, paused } => {
                let (mut header, pixels) = last_frame.expect("serving renders a frame");
                loop {
                    let wait = Wait::Forever;
                    if let Some(next) =
                        serve_commands(server, paused, wait, world, &self.gpu, &mut header, &pixels)
                    {
                        return Ok(next);
                    }
                }
            }
        }
    }
}

/// When `serve_commands` waits for commands.
enum Wait {
    WhilePaused,
    Forever,
}

/// Applies commands from pages until there are none left and `wait` allows
/// returning. Returns what to do next if a command stops the composition.
///
/// When pausing or resuming, the current frame is sent again so pages see the
/// change.
fn serve_commands(
    server: &Server,
    paused: &mut bool,
    wait: Wait,
    world: World,
    gpu: &Gpu,
    header: &mut FrameHeader,
    pixels: &[u8],
) -> Option<Next> {
    loop {
        let command = match server.try_command() {
            Some(command) => command,
            None if *paused || matches!(wait, Wait::Forever) => server.wait_command(),
            None => return None,
        };

        match command {
            Command::NewSeed => {
                return Some(Next::Rebuild(World {
                    seed: random(),
                    ..world
                }))
            }
            Command::Restart => return Some(Next::Rebuild(world)),
            Command::SetPaused(new_paused) => {
                *paused = new_paused;
                header.paused = new_paused;
                server.publish(header, pixels);
            }
            Command::Rebuild(requested) => match checked_world(requested, gpu) {
                Ok(world) => return Some(Next::Rebuild(world)),
                Err(e) => eprintln!("Ignoring a request to rebuild: {}", e),
            },
        }
    }
}

/// Converts a world requested by a page, if it can be rendered.
fn checked_world(requested: protocol::World, gpu: &Gpu) -> Result<World> {
    ensure!(
        requested.scale.is_finite() && requested.scale > 0.,
        "the scale must be greater than 0, but is {}",
        requested.scale
    );
    ensure!(
        requested.framerate >= 1,
        "the frame rate must be at least 1, but is {}",
        requested.framerate
    );
    let world = World {
        seed: requested.seed,
        width: requested.width,
        height: requested.height,
        scale: requested.scale,
        frames: requested
            .frames
            .map(|frames| usize::try_from(frames).unwrap_or(usize::MAX)),
        framerate: requested.framerate as usize,
    };
    let (output_width, output_height) = world.output_size();
    gpu.check_target_size(output_width, output_height)?;
    Ok(world)
}

impl From<World> for protocol::World {
    fn from(world: World) -> Self {
        Self {
            seed: world.seed,
            width: world.width,
            height: world.height,
            scale: world.scale,
            frames: world.frames.map(|frames| frames as u64),
            framerate: world.framerate.try_into().unwrap_or(u32::MAX),
        }
    }
}
