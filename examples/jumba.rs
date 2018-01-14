extern crate valora;

use valora::*;

struct Jumba {
    period: usize,
}

impl Sketch for Jumba {
    fn sketch(&self, cfg: &SketchCfg, mut rng: rand::StdRng) -> Result<Composition> {
        Ok(Composition::new().add(
            Mesh::from(Ellipse::circle(Point::center(), 0.2))
                .with_colorer(Colorer::white())
                .with_scale_tween(Tween::Oscillation(Oscillation {
                    period: self.period,
                    phase: 0,
                })),
        ))
    }
}

fn main() {
    let jumba = Jumba { period: 300 };
    sketch(
        SketchCfg {
            size: 1080,
            root_frame_filename: Some(String::from("jumba")),
            frame_limit: jumba.period,
            ..SketchCfg::default()
        },
        jumba,
    );
}
