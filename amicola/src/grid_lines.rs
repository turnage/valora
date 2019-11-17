//! Descriptions of a raster grid.

use euclid::Rect;

pub fn horizontal_grid_lines<U>(bounds: Rect<f32, U>) -> impl Iterator<Item = isize> {
    inclusive_iter(bounds.min_y(), bounds.max_y())
}

pub fn vertical_grid_lines<U>(bounds: Rect<f32, U>) -> impl Iterator<Item = isize> {
    inclusive_iter(bounds.min_x(), bounds.max_x())
}

fn inclusive_iter(startf: f32, endf: f32) -> impl Iterator<Item = isize> {
    let start = startf.floor() as isize;
    let end = endf.ceil() as isize;

    (start..=end)
}
