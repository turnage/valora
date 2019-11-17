//! Stroker for a path.

use crate::{monotonics::*, path::Path};

fn offset_segment(segment: &Segment, offset: f32) -> impl Iterator<Item = Segment> {
    match segment {
        Segment::LineSegment(line_segment) => std::iter::once(Segment::from(
            line_segment.translate(line_segment.normal(), offset),
        )),
        _ => unimplemented!(),
    }
}

pub fn stroke_path(path: &Path, offset: f32) -> impl Iterator<Item = Segment> {
    let segments = SegmentSet::build_from_path(&path);
    let inner_path = segments
        .iter()
        .flat_map(|s| offset_segment(s, -offset))
        .collect::<Vec<Segment>>()
        .into_iter();
    let outer_path = segments.iter().flat_map(|s| offset_segment(s, offset));
    outer_path
        .chain(inner_path.rev())
        .collect::<Vec<Segment>>()
        .into_iter()
}
