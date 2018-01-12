use point::Point;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Ngon {
    pub n:      usize,
    pub phase:  f32,
    pub radius: f32,
    pub center: Point,
}
