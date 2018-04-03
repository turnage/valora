use palette::{Alpha, Hsva, Limited, Rgb, RgbHue};

pub type Color = Alpha<Rgb, f32>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BlendMode {
    Normal,
    Add,
    Subtract,
    MaskOpaque,
    MaskTransparent,
}

pub fn hsva(h: f32, s: f32, v: f32, a: f32) -> Color {
    Hsva::new(RgbHue::from(h), s, v, a).into()
}

pub fn hsv(h: f32, s: f32, v: f32) -> Color {
    hsva(h, s, v, 1.0)
}

pub fn uniform_palette(
    start: f32,
    length: f32,
    saturation: f32,
    value: f32,
    alpha: f32,
    count: usize,
) -> Vec<Color> {
    let interval = length / (count as f32);
    (0..count)
        .into_iter()
        .map(|i| hsva(start + (i as f32) * interval, saturation, value, alpha))
        .collect()
}

pub mod conversions {
    use image;
    use palette;

    pub fn collapse(raw: palette::Rgb) -> image::Rgb<u8> {
        image::Rgb {
            data: [
                collapse_component(raw.red),
                collapse_component(raw.green),
                collapse_component(raw.blue),
            ],
        }
    }

    pub fn collapse_component(c: f32) -> u8 {
        (c * 255.0) as u8
    }
}
