pub mod poly;
pub mod ellipse;
pub mod point;
pub mod line;
pub mod spawner;

pub use self::ellipse::*;
pub use self::line::*;
pub use self::point::*;
pub use self::poly::*;
pub use self::spawner::*;

pub trait Percent: Sized {
    fn percent(self, percent: f32) -> Self;
}

pub trait SubdivideEdges: Sized {
    // Cuts the edges of the geometry in half so that points exist at the
    // midpoint of all previously existing edges. The actual shape should
    // not change.
    fn subdivide_edges(self) -> Self;

    fn subdivide_edges_n(self, n: usize) -> Self {
        (0..n).into_iter().fold(self, |s, _| s.subdivide_edges())
    }
}

pub trait Distance<Dest> {
    fn distance(&self, dest: Dest) -> f32;
    fn delta(&self, dest: Dest) -> Point;
    fn midpoint(&self, dest: Dest) -> Point;
    fn manhattan_distance(&self, dest: Dest) -> f32;
}

pub trait Scale {
    fn scale(self, scale: f32) -> Self;
}

pub trait Centered {
    fn centroid(&self) -> Point;
}

pub trait Place: Sized {
    fn place(self, dest: Point) -> Self;
}

pub trait Translate: Sized {
    fn translate(self, delta: Point) -> Self;
}