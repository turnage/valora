//! Canvas rendering.

use crate::{canvas::*, gpu::*, paint::*, uniforms::*, Options, Result, World};
use glium::{
    framebuffer::SimpleFrameBuffer,
    glutin::event_loop::{ControlFlow, EventLoop},
    texture::{texture2d::Texture2d, Dimensions, MipmapsOption},
    uniforms::MagnifySamplerFilter,
    Frame, GlObject, Program, Surface,
};
use glutin::platform::desktop::EventLoopExtDesktop;
use image::{ImageBuffer, Rgba};
use palette::{
    encoding::{srgb::Srgb, TransferFn},
    Component,
};
use rand::{random, rngs::StdRng};
use rayon::prelude::*;
use std::{path::PathBuf, rc::Rc, time::Duration};

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

pub enum Rebuild {
    NewSeed(u64),
}

pub struct RenderReport {
    /// Whether the user explicitly quit.
    pub explicit_quit: bool,
    /// An instruction to rebuild the render.
    pub rebuild: Option<Rebuild>,
}

#[derive(Debug)]
struct FrameUpdates {
    new_seed: Option<u64>,
    wait: Option<Duration>,
    should_quit: bool,
}

pub enum RenderStrategy<'a> {
    Screen {
        events_loop: EventLoop<()>,
        wait: Duration,
        buffer: SimpleFrameBuffer<'a>,
    },
    File {
        base_path: PathBuf,
        number_width: usize,
        buffer: Texture2d,
    },
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
pub struct Renderer<'a> {
    pub strategy: RenderStrategy<'a>,
    pub gpu: &'a Gpu,
    pub options: Options,
    pub rng: StdRng,
    pub output_width: u32,
    pub output_height: u32,
}

impl<'a> Renderer<'a> {
    /// Render all of the frames for the composition. This will not return until until all frames of
    /// the composition have been rendered.
    pub fn render_frames(
        &mut self,
        mut f: impl FnMut(Context, &mut Canvas),
    ) -> Result<RenderReport> {
        let default_shader = self.gpu.default_shader();

        let end_frame = self.options.world.frames.map(|f| f + self.options.delay);
        for frame in std::iter::successors(Some(0), move |last| {
            if let Some(end_frame) = end_frame {
                if last + 1 <= end_frame {
                    Some(last + 1)
                } else {
                    None
                }
            } else {
                Some(last + 1)
            }
        }) {
            let mut canvas = Canvas::new(default_shader.clone(), self.options.world.scale);
            f(
                Context {
                    rng: &mut self.rng,
                    world: self.options.world,
                    frame,
                    time: Duration::from_secs_f32(
                        frame as f32 / self.options.world.framerate as f32,
                    ),
                },
                &mut canvas,
            );

            let updates = self.render_frame(self.options.world.seed, frame, canvas)?;
            if updates.should_quit {
                return Ok(RenderReport {
                    explicit_quit: true,
                    rebuild: None,
                });
            }

            if let Some(wait) = updates.wait {
                std::thread::sleep(wait);
            }

            if let Some(new_seed) = updates.new_seed {
                return Ok(RenderReport {
                    explicit_quit: false,
                    rebuild: Some(Rebuild::NewSeed(new_seed)),
                });
            }
        }

        Ok(RenderReport {
            explicit_quit: false,
            rebuild: None,
        })
    }

    fn render_frame(
        &mut self,
        current_seed: u64,
        frame_number: usize,
        canvas: Canvas,
    ) -> Result<FrameUpdates> {
        let sample_depth = self
            .options
            .sample_depth
            .unwrap_or(amicola::SampleDepth::Single);

        match &mut self.strategy {
            RenderStrategy::Screen {
                events_loop,
                buffer,
                wait,
            } => {
                self.gpu.render(
                    self.output_width,
                    self.output_height,
                    canvas.elements(),
                    buffer,
                    sample_depth,
                )?;

                let rect = glium::Rect {
                    left: 0,
                    bottom: 0,
                    width: self.output_width,
                    height: self.output_height,
                };

                let blit_target = glium::BlitTarget {
                    left: 0,
                    bottom: 0,
                    width: self.output_width as i32,
                    height: self.output_height as i32,
                };

                let mut frame = self.gpu.get_frame().expect("getting frame from display");
                frame.blit_from_simple_framebuffer(
                    buffer,
                    &rect,
                    &blit_target,
                    MagnifySamplerFilter::Nearest,
                );
                frame.finish()?;

                let mut new_seed = None;
                let mut should_quit = false;
                events_loop.run_return(|event, _, control_flow| {
                    use glutin::event::{
                        DeviceEvent, ElementState, Event, KeyboardInput, VirtualKeyCode,
                        WindowEvent,
                    };
                    match event {
                        Event::DeviceEvent {
                            event:
                                DeviceEvent::Key(KeyboardInput {
                                    virtual_keycode: Some(VirtualKeyCode::Escape),
                                    ..
                                }),
                            ..
                        } => {
                            should_quit = true;
                        }
                        Event::DeviceEvent {
                            event:
                                DeviceEvent::Key(KeyboardInput {
                                    state: ElementState::Released,
                                    virtual_keycode: Some(VirtualKeyCode::R),
                                    ..
                                }),
                            ..
                        } => {
                            new_seed = Some(random());
                        }
                        Event::WindowEvent {
                            event: WindowEvent::CloseRequested,
                            ..
                        } => {
                            should_quit = true;
                        }
                        _ => {}
                    }

                    *control_flow = ControlFlow::Exit;
                });

                Ok(FrameUpdates {
                    new_seed,
                    wait: Some(*wait),
                    should_quit,
                })
            }
            RenderStrategy::File {
                base_path,
                number_width,
                buffer,
            } => {
                self.gpu.render(
                    self.output_width,
                    self.output_height,
                    canvas.elements(),
                    &mut buffer.as_surface(),
                    sample_depth,
                )?;

                if frame_number > self.options.delay {
                    let raw: glium::texture::RawImage2d<u8> = self.gpu.read_to_ram(&buffer)?;
                    let image: ImageBuffer<Rgba<u8>, Vec<u8>> = ImageBuffer::from_raw(
                        self.output_width,
                        self.output_height,
                        raw.data
                            .into_par_iter()
                            .map(|v| v.convert::<f32>())
                            .map(|v: f32| <Srgb as TransferFn>::from_linear(v))
                            .map(|v| v.convert::<u8>())
                            .collect(),
                    )
                    .unwrap();

                    image.save(output_path(
                        base_path.clone(),
                        *number_width,
                        frame_number,
                        current_seed,
                    ))?;
                }

                Ok(FrameUpdates {
                    new_seed: None,
                    wait: None,
                    should_quit: false,
                })
            }
        }
    }
}
