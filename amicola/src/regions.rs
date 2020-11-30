//! Raster region search and enumeration.

use crate::{boundary_spans::*, grid_lines::*, sampling::*, wind::*, Pixel, V2};

use float_eq::float_ne;
use itertools::Itertools;
use lyon_geom::LineSegment;
use std::{collections::BTreeSet, f64, ops::Range};

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

        segment_iter
            .filter(|(line, _)| float_ne!(line.to.y, line.from.y, rmax <= f64::EPSILON))
            .for_each(|(segment, _)| {
                let (segment_boundaries, segment_hits) = scanline_entries(segment);
                boundaries.extend(segment_boundaries);
                hits.extend(segment_hits);
                segments.push(segment);
            });

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
        let regions = self.regions();

        regions.map(move |region| match region {
            Region::Boundary { x, y } => ShadeCommand::Boundary {
                x: x,
                y: y,
                coverage: coverage(
                    V2::new(x as f32, y as f32),
                    sample_depth,
                    segments.iter().copied(),
                ) as f32,
            },
            Region::Span { start_x, end_x, y } => ShadeCommand::Span {
                x: start_x..end_x,
                y: y,
            },
        })
    }

    fn regions(self) -> impl Iterator<Item = Region> {
        let boundary_spans = SpanIter::from_boundaries(self.boundaries.clone().into_iter());
        let hits = self.hits.into_iter();
        let span_winds = wind_spans(hits, boundary_spans);

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::{RasterInput, SampleDepth};
    use geo_types::{Coordinate, MultiPolygon, Polygon};
    use lyon_path::{
        iterator::Flattened,
        math::{point, Point},
        Builder, Event,
    };
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

    fn path_to_polygon(builder: Builder, sample_depth: SampleDepth) -> Polygon<f64> {
        let samples_per_pixel: u64 = sample_depth.into();
        let path = builder.build();
        let path = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
        let path = path.filter_map(event_to_coordinate);

        let exterior = path.collect();
        Polygon::new(exterior, vec![])
    }

    fn raster(builder: Builder) -> Vec<Region> {
        let polygon = path_to_polygon(builder, SampleDepth::Single);
        let input: RasterInput = polygon.into();
        let regions = RegionList::from(input.edges());
        let mut regions = regions.regions().collect::<Vec<Region>>();

        regions.sort();

        regions
    }

    fn test_case(builder: Builder, mut expected_regions: Vec<Region>) {
        expected_regions.sort();

        assert_eq!(expected_regions, raster(builder));
    }

    #[test]
    fn early_truncation() {
        let regions = RegionList::from(
            vec![
                (
                    LineSegment {
                        from: point(1536.0, 0.0),
                        to: point(1536.0, 1950.0),
                    },
                    1,
                ),
                (
                    LineSegment {
                        from: point(0., 1950.),
                        to: point(0., 0.),
                    },
                    1,
                ),
            ]
            .into_iter(),
        );

        let mut regions: Vec<Region> = regions.regions().collect();
        regions.sort();

        println!("regions: {:#?}", regions);

        assert_eq!(
            regions.last().and_then(|r| match r {
                Region::Span { y, .. } => Some(*y),
                _ => None,
            }),
            Some(1950)
        );
    }

    #[test]
    fn rect_spans() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 1950.0));
        builder.line_to(Point::new(1536.0, 1950.0));
        builder.line_to(Point::new(1536.0, 0.0));
        builder.line_to(Point::new(0.0, 0.0));
        let regions = raster(builder);
        let spans = regions.into_iter().filter_map(|region| match region {
            Region::Span { start_x, end_x, y } => Some((start_x..end_x, y)),
            _ => None,
        });

        let mut count = 0;
        for (i, (xs, y)) in spans.enumerate() {
            count += 1;
            assert_eq!(i as isize, y);
            assert_eq!(xs, 1..1536);
        }

        assert_eq!(count, 1951);
    }

    #[test]
    fn rect_boundaries() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 200.0));
        builder.line_to(Point::new(200.0, 200.0));
        builder.line_to(Point::new(200.0, 0.0));
        builder.line_to(Point::new(0.0, 0.0));
        let regions = raster(builder);
        let boundaries: Vec<Pixel> = regions
            .into_iter()
            .filter_map(|region| match region {
                Region::Boundary { x, y } => Some(Pixel { x, y }),
                _ => None,
            })
            .collect();

        for (i, p) in boundaries.into_iter().enumerate() {
            assert_eq!(i as isize % 201, p.y, "i % 201: {:?}", i % 201);
            assert_eq!(
                p.x,
                match i > 200 {
                    true => 200,
                    false => 0,
                },
                "i: {:?}",
                i
            );
        }
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

    #[test]
    fn large_triangle_boundaries() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 5.0));
        builder.line_to(Point::new(5.0, 0.0));
        test_case(
            builder,
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 0, y: 1 },
                Boundary { x: 0, y: 2 },
                Boundary { x: 0, y: 3 },
                Boundary { x: 0, y: 4 },
                Boundary { x: 0, y: 5 },
                Boundary { x: 1, y: 3 },
                Boundary { x: 1, y: 4 },
                Boundary { x: 2, y: 2 },
                Boundary { x: 2, y: 3 },
                Boundary { x: 3, y: 1 },
                Boundary { x: 3, y: 2 },
                Boundary { x: 4, y: 0 },
                Boundary { x: 5, y: 0 },
                Span {
                    start_x: 1,
                    end_x: 2,
                    y: 2,
                },
                Span {
                    start_x: 1,
                    end_x: 3,
                    y: 1,
                },
                Span {
                    start_x: 1,
                    end_x: 4,
                    y: 0,
                },
            ],
        );
    }
}
