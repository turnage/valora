//! Raster region search and enumeration.

use crate::{
    grid_lines::*,
    monotonics::{self, Curve, Monotonicity},
    sampling::*,
    V2,
};
use float_ord::FloatOrd;
use itertools::Itertools;
use log::trace;
use std::{
    cmp::Ordering,
    collections::BTreeSet,
    hash::{Hash, Hasher},
    ops::Range,
};

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone)]
struct Hit {
    x: isize,
    y: isize,
    y_range: Range<f32>,
    segment_id: usize,
}

impl PartialOrd for Hit {
    fn partial_cmp(&self, other: &Hit) -> Option<Ordering> {
        match self.y.cmp(&other.y) {
            Ordering::Equal => Some(self.x.cmp(&other.x)),
            o => Some(o),
        }
    }
}

impl Ord for Hit {
    fn cmp(&self, other: &Hit) -> Ordering { self.partial_cmp(other).unwrap() }
}

impl PartialEq for Hit {
    fn eq(&self, other: &Hit) -> bool { self.x == other.x && self.y == other.y }
}

impl Eq for Hit {}

impl Hash for Hit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

#[derive(Debug, Default)]
pub struct RegionList {
    hits: BTreeSet<Hit>,
    segments: Vec<(Monotonicity, monotonics::Segment)>,
}

impl From<Vec<(Monotonicity, monotonics::Segment)>> for RegionList {
    fn from(segments: Vec<(Monotonicity, monotonics::Segment)>) -> Self {
        let mut hits = BTreeSet::new();

        for (segment_id, (monotonicity, segment)) in segments.iter().enumerate() {
            let bounds = segment.bounds();

            #[derive(Debug)]
            struct SegmentHit {
                t: f32,
            }

            let mut segment_hits = BTreeSet::new();
            segment_hits.insert(FloatOrd(0.0));
            segment_hits.insert(FloatOrd(1.0));

            let iter = GridLinesIter::Bounds(bounds);

            if *monotonicity == Monotonicity::OnX || *monotonicity == Monotonicity::OnXAndY {
                for horizontal_line in iter.horizontal() {
                    if let Some(intersection) = segment.sample_y(horizontal_line as f32) {
                        segment_hits.insert(FloatOrd(intersection.t));
                    }
                }
            }

            if *monotonicity == Monotonicity::OnY || *monotonicity == Monotonicity::OnXAndY {
                for vertical_line in iter.vertical() {
                    if let Some(intersection) = segment.sample_x(vertical_line as f32) {
                        segment_hits.insert(FloatOrd(intersection.t));
                    }
                }
            }

            for (y_range, hit_point) in segment_hits
                .into_iter()
                .tuple_windows::<(_, _)>()
                .filter_map(|(t0, t1)| {
                    let t0 = segment.sample_t(t0.0)?;
                    let t1 = segment.sample_t(t1.0)?;
                    Some((
                        Range {
                            start: t0.y,
                            end: t1.y,
                        },
                        (t0 + t1) / 2.,
                    ))
                })
            {
                let (x, y) = (hit_point.x.floor() as isize, hit_point.y.floor() as isize);
                let hit = Hit {
                    x,
                    y,
                    y_range,
                    segment_id,
                };
                hits.insert(hit);
            }
        }

        Self { segments, hits }
    }
}

impl RegionList {
    pub fn shade_commands(self, sample_depth: SampleDepth) -> impl Iterator<Item = ShadeCommand> {
        let segments = self.segments;
        Self::regions(self.hits).map(move |region| match region {
            Region::Boundary { x, y } => ShadeCommand::Boundary {
                x: x,
                y: y,
                coverage: coverage(
                    V2::new(x as f32, y as f32),
                    sample_depth,
                    segments.iter().map(|(_, s)| s),
                ),
            },
            Region::Span { start_x, end_x, y } => ShadeCommand::Span {
                x: start_x..end_x,
                y: y,
            },
        })
    }

    fn regions(hits: BTreeSet<Hit>) -> impl Iterator<Item = Region> {
        let mut y = 0;
        let mut last_hit = None;
        let mut winding_number = 0;
        hits.into_iter().flat_map(move |hit| {
            if hit.y != y {
                last_hit = None;
                winding_number = 0;
                y = hit.y;
            }

            let mut span = None;

            trace!("Considering new hit:");
            trace!("Last hit: {:?}", last_hit);
            trace!("New hit: {:?}", hit);
            trace!("Winding number: {:?}", winding_number);

            let is_gap_between_hits = last_hit
                .as_ref()
                .map(|last_hit: &Hit| (last_hit.x - hit.x).abs() > 1)
                .unwrap_or(false);

            let is_new_edge = last_hit
                .as_ref()
                .map(|last_hit: &Hit| {
                    last_hit.segment_id != hit.segment_id
                        && (is_gap_between_hits
                            || last_hit.y_range.contains(&hit.y_range.start)
                            || last_hit.y_range.contains(&hit.y_range.end)
                            || hit.y_range.contains(&last_hit.y_range.start)
                            || hit.y_range.contains(&last_hit.y_range.end))
                })
                .unwrap_or(true);
            if is_new_edge {
                winding_number += 1;
                trace!("Incrementing winding number; now: {:?}\n\n", winding_number);
            }

            match last_hit.take() {
                Some(last_hit) if is_new_edge && is_gap_between_hits && winding_number % 2 == 0 => {
                    span = Some(Region::Span {
                        start_x: last_hit.x + 1,
                        end_x: hit.x,
                        y: hit.y,
                    });
                }
                _ => {}
            };
            last_hit.replace(hit.clone());

            std::iter::successors(Some(Region::Boundary { x: hit.x, y: hit.y }), move |_| {
                span.take()
            })
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::path::{Path, Segment};
    use monotonics::RasterSegmentSet;
    use pretty_assertions::assert_eq;
    use std::{convert::*, iter::*};

    #[test]
    fn small_triangle_boundaries() {
        let triangle = vec![
            Segment::LineTo(V2::new(0.0, 0.0)),
            Segment::LineTo(V2::new(0.0, 2.0)),
            Segment::LineTo(V2::new(2.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&triangle));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 1, y: 0 },
                Region::Boundary { x: 0, y: 1 },
            ]
        );
    }

    #[test]
    fn small_triangle_off_screen_to_left() {
        let triangle = vec![
            Segment::LineTo(V2::new(-1.0, 0.0)),
            Segment::LineTo(V2::new(3.0, 0.0)),
            Segment::LineTo(V2::new(3.0, 3.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&triangle));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
        let triangle = vec![
            Segment::LineTo(V2::new(0.0, 0.0)),
            Segment::LineTo(V2::new(0.0, 5.0)),
            Segment::LineTo(V2::new(5.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&triangle));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
        let triangle = vec![
            Segment::LineTo(V2::new(0.0, 3.0)),
            Segment::LineTo(V2::new(4.0, 3.0)),
            Segment::LineTo(V2::new(2.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&triangle));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
        let quad = vec![
            Segment::LineTo(V2::new(3.0, 2.0)),
            Segment::LineTo(V2::new(6.0, 4.0)),
            Segment::LineTo(V2::new(4.0, 7.0)),
            Segment::LineTo(V2::new(1.0, 5.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&quad));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
                Region::Boundary { x: 4, y: 6 },
                Region::Boundary { x: 4, y: 7 }
            ]
        );
    }

    #[test]
    fn irregular_regions() {
        let irregular = vec![
            Segment::LineTo(V2::new(6.18, 5.22)),
            Segment::LineTo(V2::new(5.06, 1.07)),
            Segment::LineTo(V2::new(2.33, 2.75)),
            Segment::LineTo(V2::new(1.69, 6.31)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&irregular));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
        let irregular = vec![
            Segment::LineTo(V2::new(8.83, 7.46)),
            Segment::LineTo(V2::new(7.23, 1.53)),
            Segment::LineTo(V2::new(3.33, 3.93)),
            Segment::LineTo(V2::new(2.42, 9.02)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&irregular));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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

        let self_intersecting = vec![
            Segment::LineTo(V2::new(3.0, 5.0)),
            Segment::LineTo(V2::new(5.0, 9.0)),
            Segment::LineTo(V2::new(7.0, 2.0)),
            Segment::LineTo(V2::new(9.0, 9.0)),
            Segment::LineTo(V2::new(11.0, 5.0)),
        ]
        .into_iter()
        .collect::<Path>();

        pretty_env_logger::init();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&self_intersecting));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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

        let circle = vec![
            Segment::LineTo(V2::new(5., 0.)),
            Segment::LineTo(V2::new(0.67, 2.5)),
            Segment::LineTo(V2::new(0.67, 7.5)),
            Segment::LineTo(V2::new(5., 10.)),
            Segment::LineTo(V2::new(9.33, 7.5)),
            Segment::LineTo(V2::new(9.33, 2.5)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&circle));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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

        let subpixel_adjacency = vec![
            Segment::LineTo(V2::new(0., 0.)),
            Segment::LineTo(V2::new(0.25, 0.25)),
            Segment::LineTo(V2::new(0.5, 0.5)),
            Segment::LineTo(V2::new(0.75, 0.75)),
            Segment::LineTo(V2::new(1.0, 1.0)),
            Segment::LineTo(V2::new(5.0, 1.0)),
            Segment::LineTo(V2::new(5.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&subpixel_adjacency));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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

        let subpixel_adjacency = vec![
            Segment::LineTo(V2::new(0., 0.)),
            Segment::LineTo(V2::new(0.25, 0.25)),
            Segment::LineTo(V2::new(0.5, 0.5)),
            Segment::LineTo(V2::new(0.75, 0.75)),
            Segment::LineTo(V2::new(1.0, 1.0)),
            Segment::LineTo(V2::new(4.0, 1.0)),
            Segment::LineTo(V2::new(4.25, 0.75)),
            Segment::LineTo(V2::new(4.5, 0.5)),
            Segment::LineTo(V2::new(4.75, 0.25)),
            Segment::LineTo(V2::new(5.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&subpixel_adjacency));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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

        let subpixel_adjacency = vec![
            Segment::LineTo(V2::new(0., 0.)),
            Segment::LineTo(V2::new(1.0, 0.1)),
            Segment::LineTo(V2::new(2.0, 1.0)),
            Segment::LineTo(V2::new(3.0, 1.0)),
            Segment::LineTo(V2::new(4.0, 0.5)),
            Segment::LineTo(V2::new(5.0, 1.0)),
            Segment::LineTo(V2::new(5.0, 0.0)),
        ]
        .into_iter()
        .collect::<Path>();

        let regions = RegionList::from(RasterSegmentSet::build_from_path(&subpixel_adjacency));

        println!("Regions: {:#?}", regions);

        assert_eq!(
            RegionList::regions(regions.hits).collect::<Vec<Region>>(),
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
}
