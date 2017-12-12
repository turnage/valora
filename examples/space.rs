extern crate rand;
extern crate valora;

use rand::Rng;
use valora::element::*;
use valora::geom::*;
use valora::palette::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Space {}

impl Sketch for Space {
    fn draw(&self, _ctx: &SketchContext) -> Result<Vec<(Shader, Element)>, String> {
        let size = 0.1;
        Ok((0..1000)
               .into_iter()
               .map(|_| {
                        (Shader::constant(Colora::hsv(RgbHue::from(0.0), 0.0, 1.0, 1.0)),
                         Element::Ellipse(ellipse::Ellipse::circle(rand::random::<Point>(),
                                                                   0.005,
                                                                   0.0)))
                    })
               .chain((0..100)
                          .into_iter()
                          .map(|_| rand::random::<Point>())
                          .map(|p| {
                                   vec![Point { x: p.x + size / 2.0, y: p.y },
                                        Point { x: p.x - size / 2.0, y: p.y },
                                        Point { x: 0.5, y: 0.5 }]
                               })
                          .map(|ps| {
            (Shader::linear(|point| {
                Colora::hsv(RgbHue::from(240.0 +
                                         rand::OsRng::new().unwrap().gen_range(0.5, 1.0) *
                                         Point::center().distance(point) *
                                         -50.0),
                            1.0,
                            1.0,
                            1.0)
            }),
             Element::Poly(poly::Poly::Irregular(ps)))
        }))
               .collect())
    }
}

fn main() {
    sketch(SketchCfg { size: 700, root_frame_filename: None }, Space {}).expect("working sketch");
}
