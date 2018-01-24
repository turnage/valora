use palette::{Colora, RgbHue};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BlendMode {
    Normal,
    Add,
    Subtract,
    MaskOpaque,
    MaskTransparent,
}

pub fn uniform_palette(
    start: f32,
    length: f32,
    saturation: f32,
    value: f32,
    alpha: f32,
    count: usize,
) -> Vec<Colora> {
    let interval = length / (count as f32);
    (0..count)
        .into_iter()
        .map(|i| {
            Colora::hsv(
                RgbHue::from(start + (i as f32) * interval),
                saturation,
                value,
                alpha,
            )
        })
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
