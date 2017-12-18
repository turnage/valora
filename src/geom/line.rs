use geom::{Point, Poly, SubdivideEdges};

pub struct Line {
    vertices: Vec<Point>,
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