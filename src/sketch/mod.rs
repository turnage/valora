use composition::Composition;
use errors::Result;
use glium::glutin::WindowEvent;
use gpu::{Factory, Gpu, GpuMesh, GpuShader, Render, Shader};
use rand::{random, SeedableRng, StdRng};
use std::{fs, thread, time};
use std::rc::Rc;
use glium::{Program, Surface};
use glium::texture::Texture2d; 
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};
use poly::Rect;
use mesh::Mesh;

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
    pub frame_limit: usize,
    pub seed: Option<usize>,
    pub still: bool,
    /// quality sets the factor above the output resolution that textures and
    /// polygons will render with and be super sampled at for output. Should
    /// be a power of 2 but hey, you do you.
    pub quality: u32,
}

impl Default for SketchCfg {
    fn default() -> Self {
        Self {
            size: 400,
            root_frame_filename: None,
            frame_limit: 400,
            seed: None,
            still: false,
            quality: 1,
        }
    }
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub gpu: Rc<Gpu>,
    pub current_seed: usize,
}

impl SketchContext {
    pub fn load_shader(
        &self,
        vert_src: Option<&'static str>,
        frag_src: &'static str,
    ) -> Result<Shader> {
        Program::from_source(
            self.gpu.as_ref(),
            vert_src.unwrap_or(shader!("default.vert")),
            frag_src,
            None,
        ).map(Into::into)
            .map_err(Into::into)
    }
}

pub trait Sketch {
    fn sketch(&self, ctx: &SketchContext, rng: StdRng) -> Result<Composition>;
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, sketch: S) -> Result<()> {
    let (gpu, events_loop) = Gpu::new(cfg.size)?;
    let current_seed = cfg.seed.unwrap_or(random());
    let mut context = SketchContext {
        cfg,
        gpu: Rc::new(gpu),
        current_seed,
    };
    let mut frame = 0;
    let mut render = Render::produce(
        sketch.sketch(&context, StdRng::from_seed(&[context.current_seed]))?,
        context.gpu.clone(),
    )?;

    let mut buffers = [
        Rc::new(Texture2d::empty(
            context.gpu.as_ref(),
            context.cfg.size * context.cfg.quality,
            context.cfg.size * context.cfg.quality,
        )?),
        Rc::new(Texture2d::empty(
            context.gpu.as_ref(),
            context.cfg.size * context.cfg.quality,
            context.cfg.size * context.cfg.quality,
        )?),
    ];
    for buf in buffers.iter() {
        buf.as_ref().as_surface().clear_color(0.0, 0.0, 0.0, 1.0)
    }
    let blitter_src = (
        GpuShader::Texture(buffers[0].clone()),
        GpuMesh::produce(Mesh::from(Rect::frame()), context.gpu.clone())?,
    );
    let blitter = vec![(&blitter_src.0, &blitter_src.1)];

    let mut cycle = Gpu::events(events_loop);
    while let Some((events_loop, events)) = cycle {
        if events
            .iter()
            .find(|event| match **event {
                WindowEvent::ReceivedCharacter('r') => true,
                _ => false,
            })
            .is_some() || (context.cfg.still && frame > 30)
        {
            context.current_seed = random();
            frame = 0;
            render = Render::produce(
                sketch.sketch(&context, StdRng::from_seed(&[context.current_seed]))?,
                context.gpu.clone(),
            )?;
        }
        if !(context.cfg.still && frame > 0) {
            render = render.step(frame)?;
            context.gpu.draw_to_texture(
                [buffers[0].as_ref(), buffers[1].as_ref()],
                frame,
                render.render(),
            )?;
            context.gpu.draw(frame, blitter.clone())?;
            if let Some(ref root_frame_filename) = context.cfg.root_frame_filename {
                if frame < context.cfg.frame_limit {
                    let save_path = match context.cfg.still {
                        true => {
                            fs::create_dir_all(&root_frame_filename)?;
                            format!("{}/{}", root_frame_filename, context.current_seed)
                        }
                        false => {
                            let dir = format!("{}/{}/", root_frame_filename, context.current_seed);
                            fs::create_dir_all(&dir)?;
                            format!("{}{:08}", dir, frame)
                        }
                    };
                    context.gpu.save_frame(&save_path)?;
                }
            }
        }
        cycle = Gpu::events(events_loop);
        thread::sleep(time::Duration::from_millis(32));
        frame += 1;
    }
    Ok(())
}
