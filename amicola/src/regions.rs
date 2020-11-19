//! Raster region search and enumeration.

use crate::{grid_lines::*, sampling::*, Pixel, V2};

use itertools::Itertools;
use log::trace;
use lyon_geom::LineSegment;
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashSet},
    hash::{Hash, Hasher},
    ops::Range,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Region {
    Boundary {
        x: isize,
        y: isize,
    },
    Span {
        start_x: isize,
        end_x: isize,
        y: isize,
    },
}

/// A command to shade a pixel.
#[derive(Clone, Debug, PartialEq)]
pub enum ShadeCommand {
    /// A command to shade a pixel at the boundary of path's raster area.
    /// Coverage indicates what percentage of the pixel is covered by the
    /// raster area. This usually maps to the alpha channel.
    Boundary { x: isize, y: isize, coverage: f32 },
    /// A command to shade a span of the framebuffer. Spans are completely
    /// covered in the raster area, so the alpha channel value for spans
    /// is usually 1.0.
    Span { x: Range<isize>, y: isize },
}

#[derive(Debug, Default, Clone)]
pub struct RegionList {
    hits: BTreeSet<Hit>,
    boundaries: BTreeSet<Pixel>,
    segments: Vec<LineSegment<f64>>,
}

impl<I> From<I> for RegionList
where
    I: Iterator<Item = (LineSegment<f64>, i32)>,
{
    fn from(segment_iter: I) -> Self {
        let mut segments = vec![];
        let mut hits = BTreeSet::new();
        let mut boundaries = BTreeSet::new();

        for (_segment_id, (segment, _wind_weight)) in segment_iter
            .filter(|(line, _)| line.to.y != line.from.y)
            .enumerate()
        {
            trace!("Considering segment: {:#?}", segment);
            let (segment_boundaries, segment_hits) = scanline_entries(segment);
            boundaries.extend(segment_boundaries);
            hits.extend(segment_hits);
            segments.push(segment);
        }

        Self {
            segments,
            boundaries,
            hits,
        }
    }
}
impl RegionList {
    pub fn shade_commands(self, sample_depth: SampleDepth) -> impl Iterator<Item = ShadeCommand> {
        let segments = self.segments.clone();

        self.regions().map(move |region| match region {
            Region::Boundary { x, y } => ShadeCommand::Boundary {
                x: x,
                y: y,
                coverage: coverage(
                    V2::new(x as f32, y as f32),
                    sample_depth,
                    segments.iter().copied(),
                ),
            },
            Region::Span { start_x, end_x, y } => ShadeCommand::Span {
                x: start_x..end_x,
                y: y,
            },
        })
    }

    fn regions(self) -> impl Iterator<Item = Region> {
        let boundary_spans = BoundarySpans {
            boundaries: self.boundaries.clone().into_iter(),
            cached: None,
        };

        let mut cached = None;
        let mut hits = self.hits.into_iter();
        let mut wind = 0;
        let mut row = 0;
        let span_winds = boundary_spans.map(move |span| {
            let BoundarySpan { y, xs } = &span;
            if *y != row {
                row = *y;
                wind = 0;
            }
            while let Some(hit) = cached.take().or_else(|| hits.next()) {
                let row_end = hit.pixel.y != *y;
                if row_end {
                    cached = Some(hit);
                    break;
                }
                let span_end = !xs.contains(&hit.pixel.x);
                let gap = hit.pixel.x > xs.start;
                let hit_is_ahead = row_end || (span_end && gap);
                if hit_is_ahead {
                    cached = Some(hit);
                }

                if span_end {
                    break;
                }

                wind += 1;
            }

            let result = (wind, span);
            result
        });

        let wound_in = |wind| wind % 2 == 1;
        let potential_fill_spans = span_winds.tuple_windows::<(_, _)>();
        let fill_spans = potential_fill_spans.filter_map(move |((wind1, span1), (_, span2))| {
            if span1.y != span2.y {
                return None;
            }
            match wound_in(wind1) {
                true => Some(Region::Span {
                    start_x: span1.xs.end,
                    end_x: span2.xs.start,
                    y: span1.y,
                }),
                false => None,
            }
        });

        fill_spans.chain(
            self.boundaries
                .into_iter()
                .map(|Pixel { x, y }| Region::Boundary { x, y }),
        )
    }
}

struct BoundarySpans {
    boundaries: std::collections::btree_set::IntoIter<Pixel>,
    cached: Option<Pixel>,
}

#[derive(Debug, Clone)]
struct BoundarySpan {
    y: isize,
    xs: Range<isize>,
}

impl Iterator for BoundarySpans {
    type Item = BoundarySpan;
    fn next(&mut self) -> Option<Self::Item> {
        let (range_start, range_y) = match self.cached.take() {
            Some(Pixel { x, y }) => (x, y),
            None => {
                let Pixel { x, y } = self.boundaries.next()?;
                (x, y)
            }
        };

        let mut range_end = range_start + 1;
        while let Some(Pixel { x, y }) = self.boundaries.next() {
            if y != range_y || x != range_end {
                self.cached = Some(Pixel { x, y });
                break;
            }

            range_end += 1;
        }

        Some(BoundarySpan {
            y: range_y,
            xs: range_start..range_end,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{polygon_edges, SampleDepth};
    use geo_types::{Coordinate, MultiPolygon, Polygon};
    use lyon_path::{iterator::Flattened, math::Point, Builder, Event};
    use pretty_assertions::assert_eq;
    use std::{convert::*, iter::*};
    use Region::*;

    fn event_to_coordinate(event: Event<Point, Point>) -> Option<Coordinate<f64>> {
        let point_to_coord = |p: Point| Coordinate {
            x: p.x as f64,
            y: p.y as f64,
        };
        match event {
            Event::Line { from, to: _ } => Some(point_to_coord(from)),
            Event::End { last, .. } => Some(point_to_coord(last)),
            _ => None,
        }
    }

    fn path_to_multipolygon(builder: Builder, sample_depth: SampleDepth) -> MultiPolygon<f64> {
        let samples_per_pixel: u64 = sample_depth.into();
        let path = builder.build();
        let path = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
        let path = path.filter_map(event_to_coordinate);

        let exterior = path.collect();
        MultiPolygon::from(Polygon::new(exterior, vec![]))
    }

    fn test_case(builder: Builder, mut expected_regions: Vec<Region>) {
        let multi_polygon = path_to_multipolygon(builder, SampleDepth::Single);
        println!("multipoly: {:?}", multi_polygon);
        let regions = RegionList::from(multi_polygon.into_iter().flat_map(polygon_edges));
        let mut actual_regions = regions.regions().collect::<Vec<Region>>();

        expected_regions.sort();
        actual_regions.sort();

        assert_eq!(expected_regions, actual_regions);
    }

    #[test]
    fn small_triangle_boundaries() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 2.0));
        builder.line_to(Point::new(2.0, 0.0));
        test_case(
            builder,
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 0, y: 1 },
                Boundary { x: 0, y: 2 },
                Boundary { x: 1, y: 0 },
                Boundary { x: 1, y: 1 },
                Boundary { x: 2, y: 0 },
            ],
        );
    }
}
