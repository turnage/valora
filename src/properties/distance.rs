use geom::Point;
use properties::Centered;

pub trait Distance<Dest> {
    fn distance(&self, dest: &Dest) -> f32;
    fn delta(&self, dest: &Dest) -> Point;
    fn midpoint(&self, dest: &Dest) -> Point;
    fn manhattan_distance(&self, dest: &Dest) -> f32;
    /// Returns a unit point with the signs of the distance to dest. So,
    /// Point {x: -1, y: 1} if the dest is to the upper left of self.
    fn sign_to(&self, dest: &Dest) -> Point;
}

impl<C1: Centered, C2: Centered> Distance<C2> for C1 {
    default fn distance(&self, dest: &C2) -> f32 { self.centroid().distance(&dest.centroid()) }

    default fn delta(&self, dest: &C2) -> Point { self.centroid().delta(&dest.centroid()) }

    default fn midpoint(&self, dest: &C2) -> Point { self.centroid().midpoint(&dest.centroid()) }

    default fn manhattan_distance(&self, dest: &C2) -> f32 {
        self.centroid().manhattan_distance(&dest.centroid())
    }
    default fn sign_to(&self, dest: &C2) -> Point {
        use num::Signed;
        (dest.centroid() - self.centroid()).signum()
    }
}