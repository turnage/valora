extern crate rayon;
extern crate valora;

use rayon::prelude::*;
use std::cmp::Ordering;
use std::rc::Rc;
use valora::*;
use valora::image;
use valora::palette::*;
use valora::rand::{Rng, StdRng};
use valora::shaders;

struct Pendulum {
    frame: Vec<Vec<Rgb>>,
}

fn color_diff(c1: Rgb, c2: Rgb) -> f32 {
    (c1.red - c2.red).abs() + (c1.green - c2.green).abs() + (c1.blue - c2.blue).abs()
}

fn flower(size: usize, starts: Vec<Point>) -> Result<Vec<Vec<Rgb>>> {
    use std::collections::HashSet;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct Pixel {
        x: usize,
        y: usize,
    }

    impl Pixel {
        fn neighbors(&self, frame: &Vec<Vec<Option<Rgb>>>) -> Vec<Pixel> {
            let Pixel { x, y } = *self;
            vec![
                Pixel { x: x - 1, y: y - 1 },
                Pixel { x, y: y - 1 },
                Pixel { x: x + 1, y: y - 1 },
                Pixel { x: x - 1, y },
                Pixel { x: x + 1, y },
                Pixel { x: x - 1, y: y + 1 },
                Pixel { x, y: y + 1 },
                Pixel { x: x + 1, y: y + 1 },
            ].into_par_iter()
                .filter(|p| p.exists(frame))
                .collect()
        }

        fn exists(&self, frame: &Vec<Vec<Option<Rgb>>>) -> bool {
            frame.get(self.x).and_then(|v| v.get(self.y)).is_some()
        }

        fn neighbor_colors(&self, frame: &Vec<Vec<Option<Rgb>>>) -> Vec<Rgb> {
            self.neighbors(frame)
                .into_iter()
                .filter_map(|Pixel { x, y }| frame[x][y])
                .collect()
        }

        fn diff(&self, c: Rgb, frame: &Vec<Vec<Option<Rgb>>>) -> f32 {
            self.neighbor_colors(frame)
                .into_iter()
                .map(|v| color_diff(v, c))
                .min_by(|v1, v2| {
                    if v1 < v2 {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                })
                .unwrap_or(0.0)
        }
    }

    let mut frame = vec![vec![None; size]; size];
    let mut available = HashSet::new();

    let mut rng = StdRng::new()?;
    let mut sample = move || {
        Rgb::new(
            rng.gen_range(0.0, 1.0),
            rng.gen_range(0.0, 1.0),
            rng.gen_range(0.0, 1.0),
        )
    };

    let clamp = |v| if v >= size { size - 1 } else { v };
    for start in starts {
        let (x, y) = (
            clamp((start.x * size as f32) as usize),
            clamp((start.y * size as f32) as usize),
        );

        frame[x][y] = Some(sample());

        for neighbor in (Pixel { x, y }).neighbors(&frame) {
            available.insert(neighbor);
        }
    }

    let mut color = sample();
    let mut added = 0;
    let mut checkpoint = 0.0;
    while let Some(best) = available
        .par_iter()
        .min_by(|p1, p2| {
            if p1.diff(color, &frame) < p2.diff(color, &frame) {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        })
        .map(|v| *v)
    {
        frame[best.x][best.y] = Some(color);
        available.remove(&best);
        for neighbor in best.neighbors(&frame) {
            if let None = frame[neighbor.x][neighbor.y] {
                available.insert(neighbor);
            }
        }
        color = sample();

        added += 1;
        if added as f32 / (size * size) as f32 > checkpoint + 0.1 {
            checkpoint += 0.1;
            println!("{:?}% done.", checkpoint * 100.0);
        }
    }

    Ok(frame
        .into_iter()
        .map(|v| {
            v.into_iter()
                .map(|v| v.unwrap_or(Rgb::new(0.0, 0.0, 0.0)))
                .collect()
        })
        .collect())
}

impl Sketch for Pendulum {
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let image = image::ImageBuffer::from_fn(ctx.cfg.size, ctx.cfg.size, |x, y| {
            valora::color::conversions::collapse(self.frame[x as usize][y as usize])
        });
        let texture: shaders::TextureShader = ctx.produce(shaders::TextureShaderSpec {
            tex: Rc::new(image),
        })?;
        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(
                RgbHue::from(49.0),
                0.2,
                1.0,
                1.0,
            )))
            .add(texture))
    }
}

fn main() {
    sketch(
        SketchCfg {
            size:                400,
            root_frame_filename: Some(String::from("flower")),
            seed:                None,
            still:               true,
        },
        Pendulum {
            frame: flower(400, vec![Point::center()]).expect(""),
        },
    ).expect("sketch");
}
