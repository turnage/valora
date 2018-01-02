extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::{Rng, SeedableRng, StdRng};

struct Pendulum;

impl Sketch for Pendulum {
    fn sketch(&self, _ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let colorer = Colorer::from(Colora::hsv(RgbHue::from(0.0), 0.73, 0.74, 1.0));
        let tengon = Ngon { n: 10, center: Point::center(), rotation: 0.0, radius: 0.2 };
        let splotcher = WaterColor::new(tengon.as_irregular(),
                                        WaterColorCfg {
                                            spread: rng.gen_range(0.07, 0.4),
                                            layers: rng.gen_range(20, 100),
                                            subdivides_per: 1,
                                            anchor_layer: false,
                                            depth: 7,
                                            color: Colora::hsv(RgbHue::from(0.0),
                                                               0.73,
                                                               0.74,
                                                               rng.gen_range(0.2, 0.7)),
                                            draw_mode: DrawMode::Fill,
                                            ..WaterColorCfg::default()
                                        },
                                        &mut rng);
        let splotch = generate(&splotcher, rng);

        Ok(Composition::new()
               .solid_layer(Colorer::from(Colora::hsv(RgbHue::from(49.0), 0.2, 1.0, 1.0)))
               .add(splotch))
    }
}

fn main() {
    sketch(SketchCfg { size: 1080, root_frame_filename: Some("ngon".to_string()), seed: None },
           Pendulum {})
            .expect("sketch");
}
