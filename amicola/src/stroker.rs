//! Stroker for a path.

use lyon_geom::LineSegment;

pub fn stroke_path(
    path: impl Iterator<Item = LineSegment<f32>>,
    thickness: f32,
) -> impl Iterator<Item = LineSegment<f32>> {
    path
}
