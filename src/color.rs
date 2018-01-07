use geom::Point;
use palette::Colora;
use std::rc::Rc;

#[derive(Copy, Clone, Debug)]
pub enum BlendMode {
    Normal,
    Add,
    Subtract,
    MaskOpaque,
    MaskTransparent,
}

pub trait Opacity {
    fn opacity(self, opacity: f32) -> Self;
}

#[derive(Clone)]
pub struct Colorer(Option<Rc<Fn(Point) -> Colora>>);

impl Default for Colorer {
    fn default() -> Self { Colorer(None) }
}

impl Opacity for Colorer {
    fn opacity(self, opacity: f32) -> Self {
        Colorer(Some(Rc::new(move |point| Colora { alpha: opacity, ..self.color(point) })))
    }
}

impl From<Colora> for Colorer {
    fn from(color: Colora) -> Self { Colorer(Some(Rc::new(move |_| color))) }
}

impl<F: 'static + Fn(Point) -> Colora> From<F> for Colorer {
    default fn from(f: F) -> Self { Colorer(Some(Rc::new(f))) }
}

impl<F: 'static + Fn(Point) -> Colora> From<Rc<F>> for Colorer {
    fn from(f: Rc<F>) -> Self { Colorer(Some(f.clone())) }
}

impl Colorer {
    pub fn red() -> Self { Self::from(Colora::rgb(1.0, 0.0, 0.0, 1.0)) }
    pub fn blue() -> Self { Self::from(Colora::rgb(0.0, 0.0, 1.0, 1.0)) }
    pub fn black() -> Self { Self::from(Colora::rgb(0.0, 0.0, 0.0, 1.0)) }
    pub fn white() -> Self { Self::from(Colora::rgb(1.0, 1.0, 1.0, 1.0)) }
    pub fn empty() -> Self { Self::from(Colora::rgb(1.0, 1.0, 1.0, 0.0)) }

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
            data: [collapse_component(raw.red),
                   collapse_component(raw.green),
                   collapse_component(raw.blue)],
        }
    }

    pub fn collapse_component(c: f32) -> u8 { (c * 255.0) as u8 }
}