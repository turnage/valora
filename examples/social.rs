extern crate rand;
extern crate valora;

use rand::{Rng, StdRng};
use valora::errors::*;
use valora::geom::*;
use valora::glium;
use valora::palette::*;
use valora::raster::*;
use valora::shaders::*;
use valora::sketch::*;

struct Chain {
    sites: Vec<Point>,
    density: usize,
}

impl Chain {
    pub fn new(bounds: &Rect, density: usize, interval: f32, rng: &mut StdRng) -> Self {
        let n = (bounds.height / interval).floor() as usize;
        Chain {
            sites: (0..n)
                .into_iter()
                .map(|i| {
                         Point {
                             y: i as f32 * interval + bounds.bottom_left.y,
                             x: rng.gen_range(0.0, bounds.width) + bounds.bottom_left.x,
                         }
                     })
                .collect(),
            density,
        }
    }

    pub fn lines(&self, y: f32) -> Vec<Line> {
        (0..(self.sites.len() - 1))
            .into_iter()
            .map(|i| Line::new(self.sites[i], self.sites[i + 1]))
            .map(|line| line.translate(Point { x: 0.0, y: -y }))
            .map(|line| line.subdivides_edges_n(self.density))
            .collect()
    }
}

struct Social {
    chains: Vec<Chain>,
    spawner: Box<Spawner<Ellipse>>,
    site_shaders: Vec<Shader>,
    shader: Shader,
    debug_shader: Shader,
    bg: Shader,
    chain_height: f32,
    y: f32,
    speed: f32,
    density: usize,
}

impl Social {
    fn dots(&self, chain: &Chain, y: f32) -> Vec<Ellipse> {
        chain
            .lines(y)
            .into_iter()
            .flat_map(|line| spawn(self.spawner.as_ref(), &line))
            .collect()
    }

    fn draw_chain(&self, chain: &Chain) -> Result<Canvas> {
        let mut canvas = Canvas::new();
        let wave1 = self.dots(chain, self.y);
        let wave2 = self.dots(chain, self.y - self.chain_height);
        let connector = spawn(self.spawner.as_ref(),
                              &wave1
                                   .last()
                                   .unwrap()
                                   .connect(wave2.first().unwrap())
                                   .subdivides_edges_n(self.density));
        for dot in vec![wave1, wave2, connector]
                .into_iter()
                .flat_map(|w| w)
                .filter(|d| d.centroid().y < 0.5 && d.centroid().y > 0.0) {
            canvas.draw(&self.shader, &dot)?;
        }
        for (i, site) in chain
                .sites
                .iter()
                .enumerate()
                .map(|(i, p)| (i, *p - Point { x: 0.0, y: self.y }))
                .chain(chain
                           .sites
                           .iter()
                           .map(|p| *p - Point { x: 0.0, y: self.y - self.chain_height })
                           .enumerate())
                .filter(|&(_, p)| p.y < 0.5 && p.y > 0.0)
                .map(|(i, p)| (i, Ellipse::circle(p, 0.035, 0.0))) {
            canvas
                .draw(&self.site_shaders[i % self.site_shaders.len()], &site)?;
        }
        Ok(canvas)
    }
}

impl Sketch for Social {
    fn draw(&self, _ctx: &SketchContext) -> Result<Canvas> {
        let mut bg = Canvas::new();
        bg.draw(&self.bg, &Rect::frame())?;
        let canvases = self.chains
            .iter()
            .map(|c| self.draw_chain(c))
            .collect::<Result<Vec<Canvas>>>()?;
        Ok(canvases.into_iter().fold(bg, |acc, c| c.atop(acc)))
    }
    fn step(self,
            _ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        Ok(Some(Self { y: (self.y + self.speed) % self.chain_height, ..self }))
    }
}

impl Seed for Social {
    fn seed(ctx: &SketchContext) -> Result<Self> {
        let mut rng = ctx.rng.clone();
        let chain_height = 3.0;
        let density = 4;
        let interval = rng.gen_range(0.05, 0.8);
        Ok(Self {
               spawner: Box::new(Instancer::new(Ellipse::circle(Point::center(),
                                                                rng.gen_range(0.003, 0.007),
                                                                0.0))),
               shader: Shader::constant(Colora::rgb(0.0, 0.0, 0.0, 0.7)),
               debug_shader: Shader::constant(Colora::rgb(1.0, 1.0, 0.0, 1.0)),
               y: 0.0,
               site_shaders: vec![Colora::hsv(RgbHue::from(202.0), 0.41, 0.69, 1.0),
                                  Colora::hsv(RgbHue::from(6.0), 0.6, 0.81, 1.0),
                                  Colora::hsv(RgbHue::from(47.0), 1.0, 0.93, 1.0),
                                  Colora::hsv(RgbHue::from(157.0), 0.29, 0.59, 1.0)]
                       .into_iter()
                       .map(|c| Shader::constant(c))
                       .collect(),
               chains: (0..(rng.gen_range(1, 10)))
                   .into_iter()
                   .map(|_| {
                            Chain::new(&Rect::new(Point { x: 0.2, y: rng.gen_range(0.0, 0.5) },
                                                  0.6,
                                                  chain_height),
                                       density,
                                       interval,
                                       &mut rng)
                        })
                   .collect(),
               chain_height,
               speed: 0.01,
               density,
               bg: Shader::constant(Colora::rgb(1.0, 0.9, 0.8, 1.0)),
           })
    }
}

fn main() {
    sketch::<Social>(SketchCfg { size: 700, seed: None, root_frame_filename: None })
        .expect("working sketch");
}