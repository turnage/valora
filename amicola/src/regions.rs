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
    x: f32,
    pixel_x: isize,
    pixel_y: isize,
}

fn epsilon_equal(a: f32, b: f32) -> bool {
    (a - b).abs() <= std::f32::EPSILON
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
    t: f32,
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
    segments: Vec<LineSegment<f32>>,
}

impl<I> From<I> for RegionList
where
    I: Iterator<Item = LineSegment<f32>>,
{
    fn from(segment_iter: I) -> Self {
        let mut segments = vec![];
        let mut hits = BTreeSet::new();
        let mut boundaries = BTreeSet::new();

        for (segment_id, segment) in segment_iter
            .filter(|line| line.to.y != line.from.y)
            .enumerate()
        {
            trace!("Considering segment: {:#?}", segment);

            let bounds = segment.bounding_rect();

            let mut segment_hits = BTreeSet::new();
            segment_hits.insert(RawHit { t: 0., axis: None });
            segment_hits.insert(RawHit { t: 1., axis: None });

            for horizontal_line in horizontal_grid_lines(bounds) {
                let y = horizontal_line as f32;
                if let Some(t) = segment.horizontal_line_intersection_t(y) {
                    segment_hits.insert(RawHit {
                        t,
                        axis: Some(Axis::X(horizontal_line)),
                    });
                }
            }

            for vertical_line in vertical_grid_lines(bounds) {
                let x = vertical_line as f32;
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

        self.regions2().map(move |region| match region {
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

    fn regions2(self) -> impl Iterator<Item = Region> {
        let boundary_spans = BoundarySpans {
            boundaries: self.boundaries.clone().into_iter(),
            cached: None,
        };

        let mut cached = None;
        let mut hits = self.hits.into_iter();
        let mut wind = 0;
        let span_winds = boundary_spans.map(move |span| {
            let BoundarySpan { y, xs } = &span;
            let mut row_end = false;
            while let Some(hit) = cached.take().or_else(|| hits.next()) {
                row_end = hit.pixel_y != *y;
                let span_end = !xs.contains(&hit.pixel_x);
                let hit_is_ahead = row_end || (span_end && hit.pixel_x > xs.start);
                if hit_is_ahead {
                    cached = Some(hit);
                }

                if row_end || span_end {
                    break;
                }

                wind += 1;
            }

            let result = (wind, span);
            if row_end {
                wind = 0;
            }

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
    use crate::segment;
    use lyon_path::{iterator::Flattened, math::Point, Builder};
    use pretty_assertions::assert_eq;
    use std::{convert::*, iter::*};

    #[test]
    fn small_triangle_boundaries() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 2.0));
        builder.line_to(Point::new(2.0, 0.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 1, y: 0 },
                Region::Boundary { x: 0, y: 1 },
            ]
        );
    }

    #[test]
    fn small_triangle_off_screen_to_left() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(-1.0, 0.0));
        builder.line_to(Point::new(3.0, 0.0));
        builder.line_to(Point::new(3.0, 3.0));
        builder.line_to(Point::new(-1.0, 0.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: -1, y: 0 },
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 3, y: 0 },
                Region::Span {
                    start_x: 1,
                    end_x: 3,
                    y: 0
                },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 1, y: 1 },
                Region::Boundary { x: 3, y: 1 },
                Region::Span {
                    start_x: 2,
                    end_x: 3,
                    y: 1
                },
                Region::Boundary { x: 1, y: 2 },
                Region::Boundary { x: 2, y: 2 },
                Region::Boundary { x: 3, y: 2 }
            ]
        );
    }

    #[test]
    fn triangle_regions() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 0.0));
        builder.line_to(Point::new(0.0, 5.0));
        builder.line_to(Point::new(5.0, 0.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 4, y: 0 },
                Region::Span {
                    start_x: 1,
                    end_x: 4,
                    y: 0,
                },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 3, y: 1 },
                Region::Span {
                    start_x: 1,
                    end_x: 3,
                    y: 1,
                },
                Region::Boundary { x: 0, y: 2 },
                Region::Boundary { x: 2, y: 2 },
                Region::Span {
                    start_x: 1,
                    end_x: 2,
                    y: 2,
                },
                Region::Boundary { x: 0, y: 3 },
                Region::Boundary { x: 1, y: 3 },
                Region::Boundary { x: 0, y: 4 }
            ]
        );
    }

    #[test]
    fn inverted_triangle_regions() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(0.0, 3.0));
        builder.line_to(Point::new(4.0, 3.0));
        builder.line_to(Point::new(2.0, 0.0));
        builder.line_to(Point::new(0.0, 3.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 1, y: 0 },
                Region::Boundary { x: 2, y: 0 },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 1, y: 1 },
                Region::Boundary { x: 2, y: 1 },
                Region::Boundary { x: 3, y: 1 },
                Region::Boundary { x: 0, y: 2 },
                Region::Boundary { x: 3, y: 2 },
                Region::Span {
                    start_x: 1,
                    end_x: 3,
                    y: 2,
                },
            ]
        );
    }

    #[test]
    fn quadrilateral_regions() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(3.0, 2.0));
        builder.line_to(Point::new(6.0, 4.0));
        builder.line_to(Point::new(4.0, 7.0));
        builder.line_to(Point::new(1.0, 5.0));
        builder.line_to(Point::new(3.0, 2.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 2, y: 2 },
                Region::Boundary { x: 3, y: 2 },
                Region::Boundary { x: 4, y: 2 },
                Region::Boundary { x: 1, y: 3 },
                Region::Boundary { x: 2, y: 3 },
                Region::Boundary { x: 4, y: 3 },
                Region::Span {
                    start_x: 3,
                    end_x: 4,
                    y: 3
                },
                Region::Boundary { x: 5, y: 3 },
                Region::Boundary { x: 1, y: 4 },
                Region::Boundary { x: 5, y: 4 },
                Region::Span {
                    start_x: 2,
                    end_x: 5,
                    y: 4
                },
                Region::Boundary { x: 1, y: 5 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 4, y: 5 },
                Region::Span {
                    start_x: 3,
                    end_x: 4,
                    y: 5
                },
                Region::Boundary { x: 5, y: 5 },
                Region::Boundary { x: 2, y: 6 },
                Region::Boundary { x: 3, y: 6 },
                Region::Boundary { x: 4, y: 6 }
            ]
        );
    }

    #[test]
    fn irregular_regions() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(6.18, 5.22));
        builder.line_to(Point::new(5.06, 1.07));
        builder.line_to(Point::new(2.33, 2.75));
        builder.line_to(Point::new(1.69, 6.31));
        builder.line_to(Point::new(6.18, 5.22));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 3, y: 1 },
                Region::Boundary { x: 4, y: 1 },
                Region::Boundary { x: 5, y: 1 },
                Region::Boundary { x: 2, y: 2 },
                Region::Boundary { x: 3, y: 2 },
                Region::Boundary { x: 5, y: 2 },
                Region::Span {
                    start_x: 4,
                    end_x: 5,
                    y: 2
                },
                Region::Boundary { x: 2, y: 3 },
                Region::Boundary { x: 5, y: 3 },
                Region::Span {
                    start_x: 3,
                    end_x: 5,
                    y: 3
                },
                Region::Boundary { x: 1, y: 4 },
                Region::Boundary { x: 2, y: 4 },
                Region::Boundary { x: 5, y: 4 },
                Region::Span {
                    start_x: 3,
                    end_x: 5,
                    y: 4
                },
                Region::Boundary { x: 6, y: 4 },
                Region::Boundary { x: 1, y: 5 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 3, y: 5 },
                Region::Boundary { x: 4, y: 5 },
                Region::Boundary { x: 5, y: 5 },
                Region::Boundary { x: 6, y: 5 },
                Region::Boundary { x: 1, y: 6 },
                Region::Boundary { x: 2, y: 6 }
            ]
        );
    }

    #[test]
    fn irregular_regions_2() {
        let mut builder = Builder::new();
        builder.move_to(Point::new(8.83, 7.46));
        builder.line_to(Point::new(7.23, 1.53));
        builder.line_to(Point::new(3.33, 3.93));
        builder.line_to(Point::new(2.42, 9.02));
        builder.line_to(Point::new(8.83, 7.46));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 6, y: 1 },
                Region::Boundary { x: 7, y: 1 },
                Region::Boundary { x: 4, y: 2 },
                Region::Boundary { x: 5, y: 2 },
                Region::Boundary { x: 6, y: 2 },
                Region::Boundary { x: 7, y: 2 },
                Region::Boundary { x: 3, y: 3 },
                Region::Boundary { x: 4, y: 3 },
                Region::Boundary { x: 7, y: 3 },
                Region::Span {
                    start_x: 5,
                    end_x: 7,
                    y: 3
                },
                Region::Boundary { x: 3, y: 4 },
                Region::Boundary { x: 7, y: 4 },
                Region::Span {
                    start_x: 4,
                    end_x: 7,
                    y: 4
                },
                Region::Boundary { x: 8, y: 4 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 3, y: 5 },
                Region::Boundary { x: 8, y: 5 },
                Region::Span {
                    start_x: 4,
                    end_x: 8,
                    y: 5
                },
                Region::Boundary { x: 2, y: 6 },
                Region::Boundary { x: 8, y: 6 },
                Region::Span {
                    start_x: 3,
                    end_x: 8,
                    y: 6
                },
                Region::Boundary { x: 2, y: 7 },
                Region::Boundary { x: 6, y: 7 },
                Region::Span {
                    start_x: 3,
                    end_x: 6,
                    y: 7
                },
                Region::Boundary { x: 7, y: 7 },
                Region::Boundary { x: 8, y: 7 },
                Region::Boundary { x: 2, y: 8 },
                Region::Boundary { x: 3, y: 8 },
                Region::Boundary { x: 4, y: 8 },
                Region::Boundary { x: 5, y: 8 },
                Region::Boundary { x: 6, y: 8 },
                Region::Boundary { x: 2, y: 9 }
            ]
        );
    }

    #[test]
    fn self_intersecting_pyramid() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(3.0, 5.0));
        builder.line_to(Point::new(5.0, 9.0));
        builder.line_to(Point::new(7.0, 2.0));
        builder.line_to(Point::new(9.0, 9.0));
        builder.line_to(Point::new(11.0, 5.0));
        builder.line_to(Point::new(3.0, 5.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 6, y: 2 },
                Boundary { x: 7, y: 2 },
                Boundary { x: 6, y: 3 },
                Boundary { x: 7, y: 3 },
                Boundary { x: 6, y: 4 },
                Boundary { x: 7, y: 4 },
                Boundary { x: 3, y: 5 },
                Boundary { x: 5, y: 5 },
                Span {
                    start_x: 4,
                    end_x: 5,
                    y: 5
                },
                Boundary { x: 6, y: 5 },
                Boundary { x: 7, y: 5 },
                Boundary { x: 8, y: 5 },
                Boundary { x: 10, y: 5 },
                Span {
                    start_x: 9,
                    end_x: 10,
                    y: 5
                },
                Boundary { x: 3, y: 6 },
                Boundary { x: 5, y: 6 },
                Span {
                    start_x: 4,
                    end_x: 5,
                    y: 6
                },
                Boundary { x: 8, y: 6 },
                Boundary { x: 10, y: 6 },
                Span {
                    start_x: 9,
                    end_x: 10,
                    y: 6
                },
                Boundary { x: 4, y: 7 },
                Boundary { x: 5, y: 7 },
                Boundary { x: 8, y: 7 },
                Boundary { x: 9, y: 7 },
                Boundary { x: 4, y: 8 },
                Boundary { x: 5, y: 8 },
                Boundary { x: 8, y: 8 },
                Boundary { x: 9, y: 8 }
            ]
        );
    }

    #[test]
    fn low_res_circle() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(5., 0.));
        builder.line_to(Point::new(0.67, 2.5));
        builder.line_to(Point::new(0.67, 7.5));
        builder.line_to(Point::new(5., 10.));
        builder.line_to(Point::new(9.33, 7.5));
        builder.line_to(Point::new(9.33, 2.5));
        builder.line_to(Point::new(5., 0.));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 3, y: 0 },
                Boundary { x: 4, y: 0 },
                Boundary { x: 5, y: 0 },
                Boundary { x: 6, y: 0 },
                //
                Boundary { x: 1, y: 1 },
                Boundary { x: 2, y: 1 },
                Boundary { x: 3, y: 1 },
                Boundary { x: 6, y: 1 },
                Span {
                    start_x: 4,
                    end_x: 6,
                    y: 1
                },
                Boundary { x: 7, y: 1 },
                Boundary { x: 8, y: 1 },
                //
                Boundary { x: 0, y: 2 },
                Boundary { x: 1, y: 2 },
                Boundary { x: 8, y: 2 },
                Span {
                    start_x: 2,
                    end_x: 8,
                    y: 2,
                },
                Boundary { x: 9, y: 2 },
                //
                Boundary { x: 0, y: 3 },
                Boundary { x: 9, y: 3 },
                Span {
                    start_x: 1,
                    end_x: 9,
                    y: 3,
                },
                //
                Boundary { x: 0, y: 4 },
                Boundary { x: 9, y: 4 },
                Span {
                    start_x: 1,
                    end_x: 9,
                    y: 4,
                },
                //
                Boundary { x: 0, y: 5 },
                Boundary { x: 9, y: 5 },
                Span {
                    start_x: 1,
                    end_x: 9,
                    y: 5,
                },
                //
                Boundary { x: 0, y: 6 },
                Boundary { x: 9, y: 6 },
                Span {
                    start_x: 1,
                    end_x: 9,
                    y: 6,
                },
                //
                Boundary { x: 0, y: 7 },
                Boundary { x: 1, y: 7 },
                Boundary { x: 8, y: 7 },
                Span {
                    start_x: 2,
                    end_x: 8,
                    y: 7,
                },
                Boundary { x: 9, y: 7 },
                //
                Boundary { x: 1, y: 8 },
                Boundary { x: 2, y: 8 },
                Boundary { x: 3, y: 8 },
                Boundary { x: 6, y: 8 },
                Span {
                    start_x: 4,
                    end_x: 6,
                    y: 8,
                },
                Boundary { x: 7, y: 8 },
                Boundary { x: 8, y: 8 },
                //
                Boundary { x: 3, y: 9 },
                Boundary { x: 4, y: 9 },
                Boundary { x: 5, y: 9 },
                Boundary { x: 6, y: 9 },
            ]
        );
    }

    #[test]
    fn subpixel_adjacency() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.line_to(Point::new(0.25, 0.25));
        builder.line_to(Point::new(0.5, 0.5));
        builder.line_to(Point::new(0.75, 0.75));
        builder.line_to(Point::new(1.0, 1.0));
        builder.line_to(Point::new(5.0, 1.0));
        builder.line_to(Point::new(5.0, 0.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 5, y: 0 },
                Span {
                    start_x: 1,
                    end_x: 5,
                    y: 0
                }
            ]
        );
    }

    #[test]
    fn double_ended_subpixel_adjacency() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.line_to(Point::new(0.25, 0.25));
        builder.line_to(Point::new(0.5, 0.5));
        builder.line_to(Point::new(0.75, 0.75));
        builder.line_to(Point::new(1.0, 1.0));
        builder.line_to(Point::new(4.0, 1.0));
        builder.line_to(Point::new(4.25, 0.75));
        builder.line_to(Point::new(4.5, 0.5));
        builder.line_to(Point::new(4.75, 0.25));
        builder.line_to(Point::new(5.0, 0.0));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 4, y: 0 },
                Span {
                    start_x: 1,
                    end_x: 4,
                    y: 0
                }
            ]
        );
    }

    #[test]
    fn complex_subpixel_adjacency() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.line_to(Point::new(1.0, 0.1));
        builder.line_to(Point::new(2.0, 1.0));
        builder.line_to(Point::new(3.0, 1.0));
        builder.line_to(Point::new(4.0, 0.5));
        builder.line_to(Point::new(5.0, 1.0));
        builder.line_to(Point::new(5.0, 0.0));
        builder.line_to(Point::new(0., 0.));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 1, y: 0 },
                Boundary { x: 3, y: 0 },
                Span {
                    start_x: 2,
                    end_x: 3,
                    y: 0
                },
                Boundary { x: 4, y: 0 },
                Boundary { x: 5, y: 0 },
            ]
        );
    }

    #[test]
    fn simple_quadratic() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.quadratic_bezier_to(Point::new(3., 3.), Point::new(2., 0.));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 1, y: 0 },
                Boundary { x: 2, y: 0 },
                Boundary { x: 1, y: 1 },
                Boundary { x: 2, y: 1 },
            ]
        );
    }

    #[test]
    fn quadratic_triangle() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.quadratic_bezier_to(Point::new(0., 4.), Point::new(2., 2.));
        builder.line_to(Point::new(2., 0.));
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 2, y: 0 },
                Span {
                    start_x: 1,
                    end_x: 2,
                    y: 0
                },
                Boundary { x: 0, y: 1 },
                Boundary { x: 2, y: 1 },
                Span {
                    start_x: 1,
                    end_x: 2,
                    y: 1
                },
                Boundary { x: 0, y: 2 },
                Boundary { x: 1, y: 2 },
            ]
        );
    }

    #[test]
    fn cubic_triangle() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.quadratic_bezier_to(Point::new(0., 4.), Point::new(2., 2.));
        builder.cubic_bezier_to(
            Point::new(2.5, 1.5),
            Point::new(1.5, 0.5),
            Point::new(2., 0.),
        );
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 1, y: 0 },
                Boundary { x: 0, y: 1 },
                Boundary { x: 2, y: 1 },
                Span {
                    start_x: 1,
                    end_x: 2,
                    y: 1
                },
                Boundary { x: 0, y: 2 },
                Boundary { x: 1, y: 2 },
            ]
        );
    }

    #[test]
    fn cubic_blob() {
        use Region::*;

        let mut builder = Builder::new();
        builder.move_to(Point::new(0., 0.));
        builder.quadratic_bezier_to(Point::new(0., 20.), Point::new(14., 16.));
        builder.cubic_bezier_to(
            Point::new(20., 12.),
            Point::new(8., 4.),
            Point::new(14., 0.),
        );
        let path = builder.build();
        let flattened_path = Flattened::new(0.1, path.into_iter());

        let regions = RegionList::from(flattened_path.filter_map(segment));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Boundary { x: 0, y: 0 },
                Boundary { x: 12, y: 0 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 0
                },
                Boundary { x: 13, y: 0 },
                Boundary { x: 0, y: 1 },
                Boundary { x: 12, y: 1 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 1,
                },
                Boundary { x: 0, y: 2 },
                Boundary { x: 12, y: 2 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 2,
                },
                Boundary { x: 0, y: 3 },
                Boundary { x: 12, y: 3 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 3,
                },
                Boundary { x: 0, y: 4 },
                Boundary { x: 12, y: 4 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 4,
                },
                Boundary { x: 0, y: 5 },
                Boundary { x: 12, y: 5 },
                Span {
                    start_x: 1,
                    end_x: 12,
                    y: 5,
                },
                Boundary { x: 13, y: 5 },
                Boundary { x: 0, y: 6 },
                Boundary { x: 13, y: 6 },
                Span {
                    start_x: 1,
                    end_x: 13,
                    y: 6,
                },
                Boundary { x: 0, y: 7 },
                Boundary { x: 13, y: 7 },
                Span {
                    start_x: 1,
                    end_x: 13,
                    y: 7,
                },
                Boundary { x: 0, y: 8 },
                Boundary { x: 1, y: 8 },
                Boundary { x: 14, y: 8 },
                Span {
                    start_x: 2,
                    end_x: 14,
                    y: 8,
                },
                Boundary { x: 1, y: 9 },
                Boundary { x: 14, y: 9 },
                Span {
                    start_x: 2,
                    end_x: 14,
                    y: 9,
                },
                Boundary { x: 1, y: 10 },
                Boundary { x: 14, y: 10 },
                Span {
                    start_x: 2,
                    end_x: 14,
                    y: 10,
                },
                Boundary { x: 15, y: 10 },
                Boundary { x: 1, y: 11 },
                Boundary { x: 2, y: 11 },
                Boundary { x: 15, y: 11 },
                Span {
                    start_x: 3,
                    end_x: 15,
                    y: 11,
                },
                Boundary { x: 2, y: 12 },
                Boundary { x: 15, y: 12 },
                Span {
                    start_x: 3,
                    end_x: 15,
                    y: 12,
                },
                Boundary { x: 2, y: 13 },
                Boundary { x: 3, y: 13 },
                Boundary { x: 15, y: 13 },
                Span {
                    start_x: 4,
                    end_x: 15,
                    y: 13,
                },
                Boundary { x: 3, y: 14 },
                Boundary { x: 4, y: 14 },
                Boundary { x: 15, y: 14 },
                Span {
                    start_x: 5,
                    end_x: 15,
                    y: 14,
                },
                Boundary { x: 4, y: 15 },
                Boundary { x: 5, y: 15 },
                Boundary { x: 6, y: 15 },
                Boundary { x: 14, y: 15 },
                Span {
                    start_x: 7,
                    end_x: 14,
                    y: 15,
                },
                Boundary { x: 15, y: 15 },
                Boundary { x: 6, y: 16 },
                Boundary { x: 7, y: 16 },
                Boundary { x: 8, y: 16 },
                Boundary { x: 9, y: 16 },
                Boundary { x: 10, y: 16 },
                Boundary { x: 11, y: 16 },
                Boundary { x: 12, y: 16 },
                Boundary { x: 13, y: 16 },
            ]
        );
    }
}
