extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::{Rng, StdRng};

struct Pendulum;

impl Sketch for Pendulum {
    fn sketch(&self, _cfg: &SketchCfg, mut rng: StdRng) -> Result<Composition> {
        let tengon = Poly::Ngon(Ngon {
            n: 10,
            center: Point::center(),
            phase: 0.0,
            radius: 0.2,
        });
        let splotcher = WaterColor::new(
            tengon,
            WaterColorCfg {
                spread: rng.gen_range(0.04, 0.8),
                layers: rng.gen_range(50, 300),
                subdivides_per: 1,
                anchor_layer: true,
                depth: 7,
                color: Colora::hsv(
                    RgbHue::from(rng.gen_range(0.0, 360.0)),
                    1.0,
                    1.0,
                    rng.gen_range(0.02, 0.07),
                ),
                draw_mode: DrawMode::Fill,
                ..WaterColorCfg::default()
            },
            &mut rng,
        );

        let splotch = generate(&splotcher, rng);

        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(
                RgbHue::from(49.0),
                0.2,
                1.0,
                1.0,
            )))
            .add(splotch))
    }
}

fn main() {
    sketch(
        SketchCfg {
            size: 500,
            root_frame_filename: Some(String::from("ngon")),
            seed: None,
            still: true,
        },
        Pendulum {},
    ).expect("sketch");
}
