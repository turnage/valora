use geom::{Centered, Point, Poly, SubdivideEdges, Translate};

#[derive(Clone, Debug)]
pub struct Line {
    vertices: Vec<Point>,
}

pub trait Connect<T> {
    fn connect(&self, end: &T) -> Line;
}

impl<C1: Centered, C2: Centered> Connect<C2> for C1 {
    default fn connect(&self, end: &C2) -> Line { Line::new(self.centroid(), end.centroid()) }
}

impl Connect<Line> for Line {
    fn connect(&self, end: &Line) -> Line {
        Line::new(*self.vertices.last().unwrap(), *end.vertices.first().unwrap())
    }
}

impl Line {
    pub fn new(start: Point, end: Point) -> Self { Self { vertices: vec![start, end] } }
}

impl SubdivideEdges for Line {
    fn subdivide_edges(self) -> Self { Self { vertices: self.vertices.subdivide_edges() } }
}

impl Poly for Line {
    fn vertices<'a>(&'a self) -> &'a [Point] { &self.vertices }
}

impl Translate for Line {
    fn translate(self, delta: Point) -> Self {
        Self {
            vertices: self.vertices()
                .into_iter()
                .map(|p| *p + delta)
                .collect(),
        }
    }
}