extern crate rand;
extern crate valora;

use rand::Rng;
use valora::errors::*;
use valora::geom::*;
use valora::palette::*;
use valora::render::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Space {
    stars: (Shader, Vec<Geometry>),
    lasers: (Shader, Vec<Geometry>),
}

impl Space {
    pub fn new() -> Self {
        let size = 0.1;
        Space {
            stars: (Shader::constant(Colora::hsv(RgbHue::from(0.0), 0.0, 1.0, 1.0)),
                    (0..1000)
                        .into_iter()
                        .map(|_| {
                                 Geometry::Ellipse(ellipse::Ellipse::circle(rand::random::<Point>(),
                                                                     0.005,
                                                                     0.0))
                             })
                        .collect()),
            lasers: (Shader::linear(|point| {
                Colora::hsv(RgbHue::from(240.0 +
                                         rand::OsRng::new().unwrap().gen_range(0.5, 1.0) *
                                         Point::center().distance(point) *
                                         -50.0),
                            1.0,
                            1.0,
                            1.0)
            }),
                     (0..100)
                         .into_iter()
                         .map(|_| rand::random::<Point>())
                         .map(|p| {
                                  vec![Point { x: p.x + size / 2.0, y: p.y },
                                       Point { x: p.x - size / 2.0, y: p.y },
                                       Point { x: 0.5, y: 0.5 }]
                              })
                         .map(|ps| Geometry::Poly(poly::Poly::Irregular(ps)))
                         .collect()),
        }
    }
}

impl Sketch for Space {
    fn draw(&self, _ctx: &SketchContext) -> Result<Render> {
        vec![&self.stars, &self.lasers]
            .into_iter()
            .fold(Ok(Render::new()), |r, g| r.and_then(|r| r.add(g)))
    }
}

impl Default for Space {
    fn default() -> Self { Self::new() }
}

fn main() {
    sketch(SketchCfg { size: 700, seed: None, root_frame_filename: None }, Space::new())
        .expect("working sketch");
}
