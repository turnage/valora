use errors::Result;
use lyon::tessellation::math::Point2D;

#[derive(Default)]
pub struct Tessellation {
    pub vertices: Vec<Point2D<f32>>,
    pub indices: Vec<u32>,
}

pub trait Tessellate {
    fn tessellate(&self) -> Result<Tessellation>;
}