//! Line inflations and polygon stroking.

use crate::Stroke;
use arrayvec::ArrayVec;
use geo_types::LineString;
use itertools::Itertools;
use lyon_geom::{math::point, LineSegment};
use nalgebra::geometry::{Point2, Rotation2};
use num_traits::{Num, NumCast};

pub fn inflate(stroke: Stroke) -> impl Iterator<Item = (LineSegment<f64>, i32)> {
    let Stroke {
        path,
        width,
        closed,
    } = stroke;
    let LineString(path) = path;
    path.into_iter()
        .tuple_windows::<(_, _)>()
        .map(|(start, end)| LineSegment {
            from: point(start.x, start.y),
            to: point(end.x, end.y),
        })
        .flat_map(move |segment| {
            ArrayVec::from([
                (project_right(segment, width / 2.), -1),
                (project_left(segment, width / 2.), 1),
            ])
        })
}

fn project_right(mut segment: LineSegment<f64>, distance: f64) -> LineSegment<f64> {
    project(segment, distance, -1. * std::f64::consts::FRAC_PI_2)
}

fn project_left(mut segment: LineSegment<f64>, distance: f64) -> LineSegment<f64> {
    project(segment, distance, std::f64::consts::FRAC_PI_2)
}

fn project(mut segment: LineSegment<f64>, distance: f64, theta: f64) -> LineSegment<f64> {
    let left_rot = Rotation2::new(theta);
    let vector = segment.from - segment.to;
    let vector = Point2::new(vector.x, vector.y);
    let translation_vector = left_rot * vector;
    let translation_vector = point(translation_vector.x, translation_vector.y);
    let translation_vector = translation_vector.to_vector();

    segment.translate(translation_vector)
}
