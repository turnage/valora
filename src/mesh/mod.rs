use color::{BlendMode, Colorer, Opacity};
use geom::{Place, Point, Scale};

#[derive(Debug, Clone, Copy)]
pub enum DrawMode {
    Fill,
    Stroke { thickness: f32 },
}

#[derive(Clone)]
pub struct Mesh<T: Clone> {
    pub src: T,
    pub colorer: Colorer,
    pub blend_mode: BlendMode,
    pub draw_mode: DrawMode,
}

impl<T: Clone> From<T> for Mesh<T> {
    fn from(src: T) -> Self {
        Self {
            src,
            colorer: Colorer::empty(),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Fill,
        }
    }
}

impl<T: Scale + Clone> Scale for Mesh<T> {
    fn scale(self, scale: f32) -> Self { Self { src: self.src.scale(scale), ..self } }
}

impl<T: Place + Clone> Place for Mesh<T> {
    fn place(self, dest: Point) -> Self { Self { src: self.src.place(dest), ..self } }
}

impl<T: Clone> Opacity for Mesh<T> {
    fn opacity(self, opacity: f32) -> Self {
        Self { colorer: self.colorer.opacity(opacity), ..self }
    }
}