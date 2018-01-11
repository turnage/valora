use point::Point;
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
    default fn distance(&self, dest: &C2) -> f32 { self.center().distance(&dest.center()) }

    default fn delta(&self, dest: &C2) -> Point { self.center().delta(&dest.center()) }

    default fn midpoint(&self, dest: &C2) -> Point { self.center().midpoint(&dest.center()) }

    default fn manhattan_distance(&self, dest: &C2) -> f32 {
        self.center().manhattan_distance(&dest.center())
    }
    default fn sign_to(&self, dest: &C2) -> Point {
        use num::Signed;
        (dest.center() - self.center()).signum()
    }
}
