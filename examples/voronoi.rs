
extern crate rand;
extern crate valora;

use rand::Rng;
use valora::element::*;
use valora::errors::*;
use valora::geom::*;
use valora::glium;
use valora::palette::*;
use valora::patterns::sparkles::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Voronoi {
    y: f32,
    element: (Shader, Poly),
    speed: f32,
    map_height: f32,
    sites: Vec<(Colora, Point)>,
}

impl Voronoi {
    fn new(speed: f32, map_height: f32, size: u32) -> Self {
        let sparkles = sparkles(100, map_height);
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
            .collect::<Vec<(Colora, Point)>>();
        let sites: Vec<(Colora, Point)> = {
            let shift = |shift: Point, sites: Vec<(Colora, Point)>| -> Vec<(Colora, Point)> {
                sites.into_iter().map(|(c, p)| (c, p + shift)).collect()
            };
            shift(Point { x: 0.0, y: -map_height }, sites.clone())
                .into_iter()
                .chain(sites.clone())
                .chain(shift(Point { x: 0.0, y: map_height }, sites.clone()))
                .collect()
        };
        Self {
            y: 0.0,
            speed,
            sites: sites.clone(),
            map_height,
            element: (Shader::voronoi(sites, size), Poly::frame()),
        }
    }
}

impl Seed for Voronoi {
    fn seed(self, ctx: &SketchContext) -> Result<Voronoi> {
        Ok(Voronoi::new(self.speed, self.map_height, ctx.cfg.size))
    }
}

impl Sketch for Voronoi {
    fn draw<'a>(&'a self, _ctx: &SketchContext) -> Result<Box<Iterator<Item = Element<'a>> + 'a>> {
        Ok(Box::new(vec![&self.element].into_iter().map(Into::into)))
    }

    fn step(self,
            ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        Ok(if (self.y - self.map_height).abs() < self.speed {
               None
           } else {
               Some(Self {
                        y: (self.y + self.speed) % self.map_height,
                        element: {
                            let sites_: Vec<(Colora, Point)> = self.sites
                                .clone()
                                .into_iter()
                                .map(|(c, p)| (c, p + Point { x: 0.0, y: self.y }))
                                .collect();
                            (Shader::voronoi(sites_, ctx.cfg.size), self.element.1)
                        },
                        ..self
                    })
           })
    }
}

fn main() {
    sketch(SketchCfg {
               size: 1080,
               seed: None,
               root_frame_filename: Some(String::from("voronoi")),
           },
           Voronoi::new(0.0035, 1.0, 1080))
            .expect("working sketch");
}