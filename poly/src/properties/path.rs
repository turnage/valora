use point::Point;

pub trait Path {
    fn path(&self, completion: f32) -> Point;
}
