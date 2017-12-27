extern crate valora;

use palette::*;
use rand::StdRng;
use valora::*;

struct Circle {
    radius: f32,
    count: usize,
    speed: f32,
}

impl Sketch for Circle {
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let dot_gen = |c: Colorer| {
            move |p: Point| Mesh { src: Ellipse::circle(p, self.radius), colorer: c.clone() }
        };
        let dots: Vec<Tween<Mesh<Ellipse>>> = [Colorer::red(), Colorer::blue()]
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
        /*let dots = Entanglement::NtoN(dots,
                                      |this, other| if this.colorer.color(this.centroid()) ==
                                                       other.colorer.color(other.centroid()) {
                                          this.move_toward(other, speed)
                                      } else {
                                          this.move_away(other, speed)
                                      });*/
        Ok(Composition::new().add(dots))
    }
}

fn main() {
    sketch(SketchCfg { size: 500, root_frame_filename: None, seed: None },
           Circle { radius: 0.05, count: 25, speed: 0.02 })
            .expect("sketch");
}