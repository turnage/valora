//! Stroker for a path.

use crate::{monotonics::RasterSegment, PathIterator};
use pathfinder_path_utils::{segments::Segment as PathFinderSegment, stroke::StrokeToFillIter};

pub fn stroke_path(path: impl PathIterator, thickness: f32) -> impl Iterator<Item = RasterSegment> {
    std::iter::empty()
}
