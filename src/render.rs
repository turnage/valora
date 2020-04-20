//! Canvas rendering.

use crate::{canvas::*, gpu::*, paint::*, uniforms::*, Options, Result, World};
use glium::{
    glutin::event_loop::{ControlFlow, EventLoop},
    texture::{texture2d::Texture2d, Dimensions, MipmapsOption},
    Frame, GlObject, Program,
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

pub enum RenderStrategy<F1, F2> {
    Screen {
        get_frame: F1,
        events_loop: EventLoop<()>,
        wait: Duration,
        texture_program: Rc<Program>,
        buffer: Texture2d,
    },
    File {
        output_path: F2,
        buffer: Texture2d,
    },
}

/// A render gate renders frames.
pub struct Renderer<'a, F1, F2> {
    pub strategy: &'a mut RenderStrategy<F1, F2>,
    pub gpu: &'a Gpu,
    pub options: Options,
    pub rng: &'a mut StdRng,
    pub output_width: u32,
    pub output_height: u32,
}

impl<'a, F1: Fn() -> Frame + 'a, F2: Fn(usize, u64) -> PathBuf> Renderer<'a, F1, F2> {
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
                    rng: self.rng,
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

        match self.strategy {
            RenderStrategy::Screen {
                get_frame,
                events_loop,
                buffer,
                texture_program,
                wait,
            } => {
                self.gpu.render(
                    self.output_width,
                    self.output_height,
                    canvas.elements(),
                    &mut buffer.as_surface(),
                    sample_depth,
                )?;

                #[derive(UniformSet)]
                struct QuadUniforms {
                    texture_in: Texture2d,
                }

                let shader = self.gpu.build_shader(
                    texture_program.clone(),
                    QuadUniforms {
                        texture_in: unsafe {
                            // Create a weak reference to the intermediate buffer, which we will draw
                            // to the frame buffer with a quad.
                            Texture2d::from_id(
                                self.gpu.ctx.get_context(),
                                TEXTURE_FORMAT,
                                buffer.get_id(),
                                /*owned=*/ false,
                                MipmapsOption::NoMipmap,
                                Dimensions::Texture2d {
                                    width: buffer.dimensions().0,
                                    height: buffer.dimensions().1,
                                },
                            )
                        },
                    },
                );
                let mut quad_canvas = Canvas::new(shader.clone(), self.options.world.scale);
                quad_canvas.paint(Filled(self.options.world));

                let mut frame = get_frame();
                frame.set_finish()?;

                self.gpu.render(
                    self.output_width,
                    self.output_height,
                    quad_canvas.elements(),
                    &mut frame,
                    amicola::SampleDepth::Single,
                )?;

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
                output_path,
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

                    image.save(output_path(frame_number, current_seed))?;
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
