//! Rasterizes a few paths into a PNG.
//!
//! Usage: cargo run --release --example render -- [output.png]

use amicola::{
    geo_types::{LineString, Polygon},
    Canvas, FillRule, Raster, SampleDepth, ShadeCommand,
};
use std::{f64::consts::PI, fs::File, io::BufWriter, time::Instant};

const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;

/// A linear RGB framebuffer.
struct Image(Vec<[f32; 3]>);

impl Image {
    /// Composites a raster over the image in the given color.
    fn paint(&mut self, raster: Raster, color: [f32; 3], alpha: f32) {
        let mut blend = |x: isize, y: isize, opacity: f32| {
            let pixel = &mut self.0[y as usize * WIDTH as usize + x as usize];
            let a = alpha * opacity;
            for (channel, c) in pixel.iter_mut().zip(color) {
                *channel = c * a + *channel * (1. - a);
            }
        };
        for command in raster {
            match command {
                ShadeCommand::Boundary { x, y, coverage } => blend(x, y, coverage),
                ShadeCommand::Span { x, y } => x.for_each(|x| blend(x, y, 1.)),
            }
        }
    }

    fn save(&self, path: &str) -> std::io::Result<()> {
        let encode = |linear: f32| {
            let srgb = match linear {
                l if l <= 0.0031308 => l * 12.92,
                l => 1.055 * l.powf(1. / 2.4) - 0.055,
            };
            (srgb.clamp(0., 1.) * 255.).round() as u8
        };
        let data: Vec<u8> = self.0.iter().flatten().map(|c| encode(*c)).collect();

        let mut encoder = png::Encoder::new(BufWriter::new(File::create(path)?), WIDTH, HEIGHT);
        encoder.set_color(png::ColorType::Rgb);
        encoder.set_depth(png::BitDepth::Eight);
        encoder.write_header()?.write_image_data(&data)?;
        Ok(())
    }
}

fn star(canvas: &mut Canvas, center: (f64, f64), radius: f64) {
    for i in 0..5 {
        let theta = -PI / 2. + i as f64 * 4. * PI / 5.;
        let point = (
            center.0 + radius * theta.cos(),
            center.1 + radius * theta.sin(),
        );
        match i {
            0 => canvas.move_to(point),
            _ => canvas.line_to(point),
        }
    }
    canvas.close_path();
}

fn main() -> std::io::Result<()> {
    let output = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "amicola.png".into());
    let start = Instant::now();

    let mut image = Image(vec![[0.92, 0.9, 0.85]; (WIDTH * HEIGHT) as usize]);
    let mut canvas = Canvas::new(WIDTH, HEIGHT);
    canvas.set_sample_depth(SampleDepth::Super64);

    // The same star under each fill rule.
    star(&mut canvas, (130., 150.), 110.);
    image.paint(canvas.fill(), [0.6, 0.05, 0.05], 1.);
    canvas.set_fill_rule(FillRule::EvenOdd);
    star(&mut canvas, (370., 150.), 110.);
    image.paint(canvas.fill(), [0.6, 0.05, 0.05], 1.);
    canvas.set_fill_rule(FillRule::NonZero);

    // A polygon with a hole, and a translucent circle overlapping it.
    let square = |x: f64, y: f64, s: f64| {
        LineString::from(vec![(x, y), (x + s, y), (x + s, y + s), (x, y + s)])
    };
    canvas.polygon(&Polygon::new(
        square(540., 40., 220.),
        vec![square(600., 100., 100.)],
    ));
    image.paint(canvas.fill(), [0.05, 0.15, 0.5], 1.);
    canvas.move_to((760., 260.));
    canvas.arc((650., 260.), (110., 70.), 2. * PI, 0.3);
    canvas.close_path();
    image.paint(canvas.fill(), [0.9, 0.6, 0.05], 0.5);

    // Translucent strokes which cross themselves; overlaps must not darken.
    canvas.set_stroke_width(24.);
    canvas.move_to((60., 330.));
    canvas.cubic_to((500., 640.), (-100., 640.), (340., 330.));
    image.paint(canvas.stroke(), [0.05, 0.35, 0.15], 0.6);
    canvas.set_stroke_width(10.);
    canvas.move_to((420., 560.));
    for i in 1..=12 {
        let x = 420. + i as f64 * 28.;
        let y = if i % 2 == 0 { 560. } else { 340. };
        canvas.line_to((x, y));
    }
    image.paint(canvas.stroke(), [0.3, 0.05, 0.4], 0.6);

    // Hairlines.
    for i in 0..8 {
        canvas.set_stroke_width(0.25 + i as f64 * 0.25);
        canvas.move_to((40. + i as f64 * 14., 450.));
        canvas.quadratic_to((120. + i as f64 * 14., 600.), (380., 580.));
        image.paint(canvas.stroke(), [0.05, 0.05, 0.05], 1.);
    }

    println!("rasterized in {:?}", start.elapsed());
    image.save(&output)
}
