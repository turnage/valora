//! Raster region search and enumeration.

use crate::{ext, grid_lines::*, sampling::*, V2};
use float_ord::FloatOrd;
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

#[derive(Debug, Clone, Copy)]
struct Hit {
    x: f64,
    pixel_x: isize,
    pixel_y: isize,
    wind_weight: i32,
}

impl PartialOrd for Hit {
    fn partial_cmp(&self, other: &Hit) -> Option<Ordering> {
        match self.pixel_y.cmp(&other.pixel_y) {
            Ordering::Equal => Some(FloatOrd(self.x).cmp(&FloatOrd(other.x))),
            o => Some(o),
        }
    }
}

impl Ord for Hit {
    fn cmp(&self, other: &Hit) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for Hit {
    fn eq(&self, other: &Hit) -> bool {
        self.pixel_x == other.pixel_x && self.pixel_y == other.pixel_y
    }
}

impl Eq for Hit {}

impl Hash for Hit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pixel_x.hash(state);
        self.pixel_y.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub enum Axis {
    X(isize),
    Y(isize),
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct RawHit {
    t: f64,
    axis: Option<Axis>,
}

impl PartialOrd for RawHit {
    fn partial_cmp(&self, other: &RawHit) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RawHit {
    fn cmp(&self, other: &RawHit) -> Ordering {
        match FloatOrd(self.t).cmp(&FloatOrd(other.t)) {
            Ordering::Equal => self.axis.cmp(&other.axis),
            ordering => ordering,
        }
    }
}

impl Eq for RawHit {}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
struct Pixel {
    x: isize,
    y: isize,
}

impl PartialOrd for Pixel {
    fn partial_cmp(&self, other: &Pixel) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pixel {
    fn cmp(&self, other: &Pixel) -> Ordering {
        match self.y.cmp(&other.y) {
            Ordering::Equal => self.x.cmp(&other.x),
            other => other,
        }
    }
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

        for (segment_id, (segment, wind_weight)) in segment_iter
            .filter(|(line, _)| line.to.y != line.from.y)
            .enumerate()
        {
            trace!("Considering segment: {:#?}", segment);

            let bounds = segment.bounding_rect();

            let mut segment_hits = BTreeSet::new();
            segment_hits.insert(RawHit { t: 0., axis: None });
            segment_hits.insert(RawHit { t: 1., axis: None });

            for horizontal_line in horizontal_grid_lines(bounds) {
                let y = horizontal_line as f64;
                if let Some(t) = segment.horizontal_line_intersection_t(y) {
                    segment_hits.insert(RawHit {
                        t,
                        axis: Some(Axis::X(horizontal_line)),
                    });
                }
            }

            for vertical_line in vertical_grid_lines(bounds) {
                let x = vertical_line as f64;
                if let Some(t) = segment.vertical_line_intersection_t(x) {
                    segment_hits.insert(RawHit {
                        t,
                        axis: Some(Axis::Y(vertical_line)),
                    });
                }
            }

            segment_hits
                .into_iter()
                .tuple_windows::<(_, _)>()
                .filter_map(|(h1, h2)| {
                    let p1 = segment.sample(h1.t);
                    let p2 = segment.sample(h2.t);
                    let midpoint = (p1 + p2.to_vector()) / 2.;

                    let x = midpoint.x.floor() as isize;
                    let y = midpoint.y.floor() as isize;
                    boundaries.insert(Pixel { x, y });

                    h1.axis.and_then(|axis| match axis {
                        Axis::X(pixel_y) => Some((p1.x, pixel_y)),
                        _ => None,
                    })
                })
                .for_each(|(x, pixel_y)| {
                    hits.insert(Hit {
                        pixel_y,
                        pixel_x: x.floor() as isize,
                        x,
                        wind_weight,
                    });
                });

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
                let row_end = hit.pixel_y != *y;
                if row_end {
                    cached = Some(hit);
                    break;
                }
                let span_end = !xs.contains(&hit.pixel_x);
                let gap = hit.pixel_x > xs.start;
                let hit_is_ahead = row_end || (span_end && gap);
                if hit_is_ahead {
                    cached = Some(hit);
                }

                if span_end {
                    break;
                }

                wind += hit.wind_weight;
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
    use lyon_path::{iterator::Flattened, math::Point, Builder, Event, Path};
    use pretty_assertions::assert_eq;
    use std::{convert::*, iter::*};
    use Region::*;

    fn event_to_coordinate(event: Event<Point, Point>) -> Option<Coordinate<f64>> {
        let point_to_coord = |p: Point| Coordinate {
            x: p.x as f64,
            y: p.y as f64,
        };
        match event {
            Event::Line { from, to } => Some(point_to_coord(from)),
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
