extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::Rng;

struct Jumba {
    period: usize,
}

impl Jumba {
    fn ring(
        &self,
        scale: f32,
        palette: &[Colora],
        forward: bool,
        rng: &mut rand::StdRng,
    ) -> (Mesh, Vec<VoronoiSite>) {
        let oscillator = Mesh::from(Ellipse::circle(Point::center(), 0.2 * scale))
            .with_colorer(Colorer::white())
            .with_blend_mode(BlendMode::Add)
            .with_scale(Tween::Oscillation(Oscillation {
                period: self.period,
                phase: 0,
            }));

        let ring_points = Ngon {
            n: palette.len(),
            phase: 0.0,
            radius: 0.3 * scale,
            center: Point::center(),
        };
        let ring = spawn(
            &Ngon {
                n: palette.len(),
                phase: 0.0,
                radius: 0.04 * scale,
                center: Point::center(),
            },
            &ring_points,
            rng.clone(),
        ).into_iter()
            .enumerate()
            .map(|(i, n)| Mesh::from(n).with_colorer(Colorer::from(palette[i])))
            .collect();
        let ring = one_to_n(&oscillator, ring, |src, r| Mesh {
            scale: src.scale.clone().chain(move |v| {
                if forward {
                    1.0 - v.abs()
                } else {
                    v.abs()
                }
            }),
            ..r
        });
        let sites: Vec<VoronoiSite> = ring.iter()
            .enumerate()
            .map(|(i, ref mesh)| VoronoiSite {
                site: mesh.src.center(),
                color: palette[i],
                strength: mesh.scale
                    .clone()
                    .chain(|v| if v == 0.0 { 0.01 } else { v }),
            })
            .collect();
        (oscillator, sites)
    }
}

impl Sketch for Jumba {
    fn sketch(&self, _: &SketchCfg, mut rng: rand::StdRng) -> Result<Composition> {
        let n = rng.gen_range(5, 10) * 2;
        let palette = uniform_palette(
            rng.gen_range(0.0, 360.0),
            rng.gen_range(1.0, 4.0) * (n as f32) * 5.0,
            1.0,
            1.0,
            1.0,
            n,
        );
        let (oscillator, mut inner_sites) = self.ring(1.0, &palette, true, &mut rng);
        let reverse_palette: Vec<Colora> = palette
            .iter()
            .skip(n / 2)
            .chain(palette.iter().take(n / 2))
            .map(|c| c.clone())
            .collect();
        let (_, mut outer_sites) = self.ring(1.5, &reverse_palette, false, &mut rng);

        let sites = {
            inner_sites.append(&mut outer_sites);
            inner_sites.push(VoronoiSite {
                site: Point::center(),
                color: Colora::rgb(1.0, 1.0, 1.0, 1.0),
                strength: Tween::Oscillation(Oscillation {
                    period: self.period / 2,
                    phase: self.period / 2,
                }).chain(|v| {
                    if v.abs() == 0.0 {
                        0.0001
                    } else {
                        v.abs()
                    }
                }),
            });

            inner_sites
        };

        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(RgbHue::from(0.0), 0.1, 1.0, 1.0)))
            .add((Shader::Voronoi(sites), Mesh::from(Rect::frame()))))
    }
}

fn main() {
    let jumba = Jumba { period: 1000 };
    sketch(
        SketchCfg {
            size: 1080,
            root_frame_filename: Some(String::from("xx")),
            frame_limit: jumba.period,
            ..SketchCfg::default()
        },
        jumba,
    ).expect("sketch");
}
