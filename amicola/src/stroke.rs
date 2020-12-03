//! Line inflations and polygon stroking.

use crate::Stroke;
use arrayvec::ArrayVec;
use geo_types::LineString;
use itertools::{Either, Itertools, Position};
use lyon_geom::{math::point, Line, LineSegment};
use nalgebra::geometry::{Point2, Rotation2};
use num_traits::{Num, NumCast};

#[derive(Debug, Copy, Clone)]
struct Projection {
    segment: LineSegment<f64>,
    line: Line<f64>,
}

pub fn inflate(stroke: Stroke) -> impl Iterator<Item = LineSegment<f64>> {
    let Stroke {
        path,
        width,
        closed,
    } = stroke;
    let (left, right) = project_path(path, width);

    let left = collapse_path(left.collect::<Vec<Projection>>(), closed);
    let right = collapse_path(right.collect::<Vec<Projection>>(), closed);

    left.chain(right.map(|segment| segment.flip()))
}

fn collapse_path(path: Vec<Projection>, closed: bool) -> impl Iterator<Item = LineSegment<f64>> {
    let mut collapsed = vec![];

    let line = |p: &Projection| p.line;

    let mut i = 1;
    let penultimate = path.len().saturating_sub(2);
    while let Some(projection) = path.get(i).cloned().filter(|_| i <= penultimate) {
        collapsed.push(collapse_line(
            path.get(i - 1).map(line),
            projection,
            path.get(i + 1).map(line),
        ));

        i += 1;
    }

    let (previous_to_first, next_after_last) = match closed {
        true => (path.last().map(line), path.first().map(line)),
        false => (None, None),
    };

    collapsed.push(collapse_line(
        previous_to_first,
        path[0],
        path.get(1).map(line),
    ));
    collapsed.push(collapse_line(
        if path.len() == 1 {
            None
        } else {
            path.get(penultimate).map(line)
        },
        path[path.len() - 1],
        next_after_last,
    ));

    collapsed.into_iter()
}

fn collapse_line(
    prev: Option<Line<f64>>,
    projection: Projection,
    next: Option<Line<f64>>,
) -> LineSegment<f64> {
    let from = prev
        .and_then(|prev| projection.line.intersection(&prev))
        .unwrap_or(projection.segment.from);
    let to = next
        .and_then(|next| projection.line.intersection(&next))
        .unwrap_or(projection.segment.to);

    LineSegment { from, to }
}

fn project_path(
    path: LineString<f64>,
    distance: f64,
) -> (
    impl Iterator<Item = Projection>,
    impl Iterator<Item = Projection>,
) {
    let LineString(path) = path;
    let project_with = move |f: fn(LineSegment<f64>, f64) -> Projection| {
        path.clone()
            .into_iter()
            .tuple_windows::<(_, _)>()
            .map(|(start, end)| LineSegment {
                from: point(start.x, start.y),
                to: point(end.x, end.y),
            })
            .map(move |segment| f(segment, distance / 2.))
    };

    (project_with(project_left), project_with(project_right))
}

fn project_right(mut segment: LineSegment<f64>, distance: f64) -> Projection {
    project(segment, distance, -1. * std::f64::consts::FRAC_PI_2)
}

fn project_left(mut segment: LineSegment<f64>, distance: f64) -> Projection {
    project(segment, distance, std::f64::consts::FRAC_PI_2)
}

fn project(mut segment: LineSegment<f64>, distance: f64, theta: f64) -> Projection {
    let left_rot = Rotation2::new(theta);
    let vector = segment.from - segment.to;
    let vector = Point2::new(vector.x, vector.y);
    let translation_vector = left_rot * vector;
    let translation_vector = point(translation_vector.x, translation_vector.y);
    let translation_vector = translation_vector.to_vector();

    let segment = segment.translate(translation_vector);
    Projection {
        segment,
        line: segment.to_line(),
    }
}
