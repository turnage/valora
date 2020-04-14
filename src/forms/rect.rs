use crate::{
    Angle, Canvas, Center, Ellipse, FlatIterPath, Paint, Rotate, Scale, Translate, P2, V2,
};
use arrayvec::ArrayVec;
use float_ord::FloatOrd;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Rect {
    pub bottom_left: P2,
    pub width: f32,
    pub height: f32,
}

impl Paint for Rect {
    fn paint(&self, c: &mut Canvas) {
        c.paint(FlatIterPath::new(self.vertices(), /*closed=*/ true));
    }
}

impl Rect {
    /// Returns a `Rect` containing all given vertices.
    pub fn extent(vertices: impl Iterator<Item = P2>) -> Self {
        let mut min_x = FloatOrd(std::f32::MAX);
        let mut min_y = FloatOrd(std::f32::MAX);
        let mut max_y = FloatOrd(0.);
        let mut max_x = FloatOrd(0.);

        for vertex in vertices {
            min_x = std::cmp::min(min_x, FloatOrd(vertex.x));
            min_y = std::cmp::min(min_y, FloatOrd(vertex.y));
            max_x = std::cmp::max(max_x, FloatOrd(vertex.x));
            max_y = std::cmp::max(max_y, FloatOrd(vertex.y));
        }

        Self {
            bottom_left: P2::new(min_x.0, min_y.0),
            width: max_x.0 - min_x.0,
            height: max_y.0 - min_y.0,
        }
    }

    pub fn vertices(&self) -> impl DoubleEndedIterator<Item = P2> + Clone {
        ArrayVec::from([
            self.bottom_left,
            self.bottom_left.translate(V2::new(0., self.height)),
            self.bottom_left.translate(V2::new(self.width, self.height)),
            self.bottom_left.translate(V2::new(self.width, 0.)),
        ])
        .into_iter()
    }
}

impl Center for Rect {
    fn center(&self) -> P2 {
        self.bottom_left
            .translate(V2::new(self.width / 2., self.height / 2.))
    }
}

impl Scale for Rect {
    fn scale(self, factor: f32) -> Self {
        let center = self.center();
        let distance = (center - self.bottom_left).length();
        let phase = Ellipse::circle(center, distance).circumphase(&self.bottom_left);
        Self {
            bottom_left: Ellipse::circle(center, distance * factor).circumpoint(phase),
            height: self.height * factor,
            width: self.width * factor,
        }
    }
}

impl Translate for Rect {
    fn translate(self, offset: V2) -> Self {
        Self {
            bottom_left: self.bottom_left.translate(offset),
            ..self
        }
    }
}
