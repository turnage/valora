# valora

A declarative generative art toolkit!

Here's a demo. (Code follows)

![demo](https://i.imgur.com/aLAJrKH.gif)

See some actual pieces I've made with it [here](https://www.instagram.com/venlute/).

````rust
extern crate valora;

use palette::*;
use rand::StdRng;
use valora::*;

struct Circle {
    radius: f32,
    count: usize,
}

impl Sketch for Circle {
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let dot_gen = |c: Colorer| {
            move |p: Point| Mesh { src: Ellipse::circle(p, self.radius), colorer: c.clone() }
        };
        let dots: Vec<Tween<Mesh<Ellipse>>> = [Colorer::from(Colora::rgb(1.0, 0.0, 0.0, 0.7)),
                                               Colorer::blue()]
                .into_iter()
                .flat_map(|c| {
                              sparkles(self.count, &Rect::frame(), &mut rng)
                                  .into_iter()
                                  .map(dot_gen(c.clone()))
                          })
                .enumerate()
                .map(|(i, mesh)| {
                    Tween::from(mesh).anim_scale(1.0,
                                                 1.5,
                                                 Interpolation::Oscillation {
                                                     oscillation: Oscillation::Sine,
                                                     start: i,
                                                     period: 100,
                                                 })
                })
                .collect();
        Ok(Composition::new()
               .add(Mesh { src: Rect::frame(), colorer: Colorer::black() })
               .add(dots))
    }
}

fn main() {
    sketch(SketchCfg { size: 500, root_frame_filename: Some(String::from("circle")), seed: None },
           Circle { radius: 0.05, count: 25 })
            .expect("sketch");
}
````