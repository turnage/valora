use palette::{Colora, RgbHue};
use poly::Point;
use std::rc::Rc;

#[derive(Copy, Clone, Debug)]
pub enum BlendMode {
    Normal,
    Add,
    Subtract,
    MaskOpaque,
    MaskTransparent,
}

#[derive(Clone)]
pub struct Colorer(Option<Rc<Fn(Point) -> Colora>>);

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
        .enumerate()
        .map(|(i, v)| {
            Colora::hsv(
                RgbHue::from(start + (i as f32) * interval),
                saturation,
                value,
                alpha,
            )
        })
        .collect()
}

impl Default for Colorer {
    fn default() -> Self {
        Colorer(None)
    }
}

impl From<Colora> for Colorer {
    fn from(color: Colora) -> Self {
        Colorer(Some(Rc::new(move |_| color)))
    }
}

impl<F: 'static + Fn(Point) -> Colora> From<F> for Colorer {
    default fn from(f: F) -> Self {
        Colorer(Some(Rc::new(f)))
    }
}

impl<F: 'static + Fn(Point) -> Colora> From<Rc<F>> for Colorer {
    fn from(f: Rc<F>) -> Self {
        Colorer(Some(f.clone()))
    }
}

impl Colorer {
    pub fn red() -> Self {
        Self::from(Colora::rgb(1.0, 0.0, 0.0, 1.0))
    }
    pub fn blue() -> Self {
        Self::from(Colora::rgb(0.0, 0.0, 1.0, 1.0))
    }
    pub fn black() -> Self {
        Self::from(Colora::rgb(0.0, 0.0, 0.0, 1.0))
    }
    pub fn white() -> Self {
        Self::from(Colora::rgb(1.0, 1.0, 1.0, 1.0))
    }
    pub fn empty() -> Self {
        Self::from(Colora::rgb(1.0, 1.0, 1.0, 0.0))
    }

    pub fn color(&self, point: Point) -> Colora {
        match self.0 {
            None => Colora::rgb(0.0, 0.0, 0.0, 0.0),
            Some(ref f) => (f.as_ref())(point),
        }
    }
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
