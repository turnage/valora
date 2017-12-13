
extern crate rand;
extern crate valora;

use rand::Rng;
use valora::errors::*;
use valora::geom::*;
use valora::glium;
use valora::palette::*;
use valora::patterns::sparkles::*;
use valora::patterns::voronoi::*;
use valora::render::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Voronoi {
    y: f32,
    sites: Vec<(Colora, Point)>,
    speed: f32,
    map_height: f32,
}

impl Voronoi {
    fn new(speed: f32, map_height: f32) -> Self {
        let sparkles = sparkles(30, map_height);
        let theme = rand::OsRng::new().unwrap().gen_range(0.0, 360.0);
        let palette = (0..8)
            .into_iter()
            .map(|h: usize| Colora::hsv(RgbHue::from(theme + ((h as f32) * 15.0)), 0.85, 1.0, 1.0))
            .collect::<Vec<Colora>>();
        let colors = sparkles
            .iter()
            .map(|_| palette[rand::OsRng::new().unwrap().gen_range(0, palette.len())])
            .collect::<Vec<Colora>>();
        let sites = colors
            .into_iter()
            .zip(sparkles.clone().into_iter())
            .collect();
        Self { y: 0.0, sites, speed, map_height }
    }
}

impl Seed for Voronoi {
    fn seed(self, _ctx: &SketchContext) -> Result<Voronoi> {
        Ok(Voronoi::new(self.speed, self.map_height))
    }
}

impl Sketch for Voronoi {
    fn draw(&self, ctx: &SketchContext) -> Result<Render> {
        let mut sites_ = self.sites
            .clone()
            .into_iter()
            .flat_map(|(c, p)| {
                let delta = (p.y - self.y) % self.map_height;
                if delta > 0.0 {
                    vec![(c, Point { x: p.x, y: delta })]
                } else {
                    vec![(c, Point { x: p.x, y: delta }),
                         (c, Point { x: p.x, y: self.map_height - delta.abs() })]
                }

            })
            .collect::<Vec<(Colora, Point)>>();
        sites_.push((Colora::hsv(RgbHue::from(0.0), 0.0, 1.0, 1.0), Point::center()));
        Ok(Render::new().add_texture(&voronoi(ctx.cfg.size, sites_)))
    }

    fn step(self,
            _ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        Ok(if (self.y - self.map_height).abs() < self.speed {
               None
           } else {
               Some(Self { y: self.y + self.speed, ..self })
           })
    }
}

fn main() {
    sketch(SketchCfg {
               size: 1080,
               seed: None,
               root_frame_filename: Some(String::from("voronoi")),
           },
           Voronoi::new(0.0035, 1.5))
            .expect("working sketch");
}