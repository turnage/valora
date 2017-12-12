use errors::Result;
use glium;
use pipeline::Pipeline;
use raster::Tessellation;
use render::{Render, Renderable};
use std::{thread, time};

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub frame: usize,
}

pub trait Sketch: Sized {
    fn draw(&self, ctx: &SketchContext) -> Result<Render>;
    fn step(self, _ctx: &SketchContext, _events: Vec<glium::glutin::WindowEvent>) -> Result<Self> {
        Ok(self)
    }
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, mut sketch: S) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size, cfg.root_frame_filename.clone())?;
    let mut context = SketchContext { cfg, frame: 0 };

    let mut cycle = pipeline.events();
    while let Ok(Some((mut pipeline, events))) = cycle {
        pipeline
            .draw(sketch
                      .draw(&context)?
                      .build()
                      .into_iter()
                      .filter_map(|r| match r {
                                      Renderable::Tessellations(t) => Some(t),
                                      _ => None,
                                  })
                      .flat_map(|ts| ts)
                      .collect::<Vec<Tessellation>>(),
                  context.frame)?;
        sketch = sketch.step(&context, events)?;
        cycle = pipeline.events();
        context.frame += 1;

        thread::sleep(time::Duration::from_millis(500));
    }
    cycle.map(|_| ())
}
