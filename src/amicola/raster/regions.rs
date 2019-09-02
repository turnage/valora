//! Raster region search and enumeration.

use super::{grid_lines::*, path::*, sampling::*};
use crate::amicola::geo::*;
use float_ord::FloatOrd;
use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};
use std::hash::{Hash, Hasher};

#[derive(Copy, Clone, Debug, PartialEq)]
enum Region {
    Boundary {
        x: isize,
        y: isize,
    },
    Fill {
        start_x: isize,
        end_x: isize,
        y: isize,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ShadeCommand {
    pub x: isize,
    pub y: isize,
    pub coverage: f64,
}

pub struct ShadeCommandIter {
    x: isize,
    y: isize,
    end_x: isize,
    coverage: f64,
}

impl Iterator for ShadeCommandIter {
    type Item = ShadeCommand;

    fn next(&mut self) -> Option<Self::Item> {
        if self.x < self.end_x {
            self.x += 1;
            Some(ShadeCommand {
                x: self.x - 1,
                y: self.y,
                coverage: self.coverage,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Hit {
    x: isize,
    y: isize,
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
    fn cmp(&self, other: &Hit) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for Hit {
    fn eq(&self, other: &Hit) -> bool {
        self.x == other.x && self.y == other.y
    }
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
    segments: Vec<MonotonicSegment>,
}

impl From<Polygon> for RegionList {
    fn from(poly: Polygon) -> Self {
        let mut list = RegionList::default();

        list.push(poly);

        list
    }
}

impl RegionList {
    fn push(&mut self, poly: Polygon) {
        let path: Vec<MonotonicSegment> = poly
            .edges()
            .map(MonotonicSegment::try_from)
            .filter_map(Result::ok)
            .collect();

        for (segment_id, segment) in path.iter().enumerate() {
            let bounds = segment.bounds();

            #[derive(Debug)]
            struct SegmentHit {
                t: f64,
            }

            let mut segment_hits = BTreeSet::new();
            segment_hits.insert(FloatOrd(0.0));
            segment_hits.insert(FloatOrd(1.0));

            let iter = GridLinesIter::Bounds(bounds);

            for horizontal_line in iter.horizontal() {
                if let Some(intersection) = segment.sample_y(horizontal_line as f64) {
                    let floor = intersection.axis.floor();
                    segment_hits.insert(FloatOrd(intersection.t));
                }
            }

            for vertical_line in iter.vertical() {
                if let Some(intersection) = segment.sample_x(vertical_line as f64) {
                    segment_hits.insert(FloatOrd(intersection.t));
                }
            }

            for hit_point in segment_hits
                .into_iter()
                .tuple_windows::<(_, _)>()
                .filter_map(|(t0, t1)| segment.sample_t((t0.0 + t1.0) / 2.0))
            {
                let (x, y) = (hit_point.x.floor() as isize, hit_point.y.floor() as isize);
                let hit = Hit { x, y, segment_id };
                self.hits.insert(hit);
            }
        }

        self.segments = path;
    }

    pub fn shade_commands<'a>(&'a self) -> impl Iterator<Item = ShadeCommand> + 'a {
        self.regions().flat_map(move |region| match region {
            Region::Boundary { x, y } => ShadeCommandIter {
                x,
                y,
                end_x: x + 1,
                coverage: coverage(
                    V2::new(x as f64, y as f64),
                    SampleDepth::Super64,
                    &self.segments,
                ),
            },
            Region::Fill {
                start_x, end_x, y, ..
            } => ShadeCommandIter {
                x: start_x,
                y,
                end_x,
                coverage: 1.0,
            },
        })
    }

    fn regions<'a>(&'a self) -> impl Iterator<Item = Region> + 'a {
        let mut y = 0;
        let mut last_hit = None;
        self.hits.iter().flat_map(move |hit| {
            if hit.y != y {
                last_hit = None;
                y = hit.y;
            }

            let mut fill = None;

            if last_hit
                .as_ref()
                .map(|b: &Hit| {
                    // Each segment can only increment the winding number once.
                    // There must also be a gap to fill.
                    b.segment_id == hit.segment_id || difference(b.x, hit.x) <= 1
                })
                .unwrap_or(false)
            {
                last_hit.replace(*hit);
            } else if let Some(last_hit) = last_hit.take() {
                fill = Some(Region::Fill {
                    start_x: last_hit.x + 1,
                    end_x: hit.x,
                    y: hit.y,
                });
            } else {
                last_hit = Some(*hit);
            }

            std::iter::successors(Some(Region::Boundary { x: hit.x, y: hit.y }), move |_| {
                fill.take()
            })
        })
    }

    fn scan_column(&self, x: f64) -> isize {
        x.floor() as isize
    }
}

fn difference(a: isize, b: isize) -> isize {
    if a > b {
        a - b
    } else {
        b - a
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //use pretty_assertions::assert_eq;
    use std::convert::*;
    use std::iter::*;

    #[test]
    fn small_triangle_boundaries() {
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 2.0),
            V2::new(2.0, 0.0),
        ])
        .expect("triangle");

        let regions = RegionList::from(triangle);

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
        let triangle = Polygon::try_from(vec![
            V2::new(-1.0, 0.0),
            V2::new(3.0, 0.0),
            V2::new(3.0, 3.0),
        ])
        .expect("triangle");

        let regions = RegionList::from(triangle);

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: -1, y: 0 },
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 3, y: 0 },
                Region::Fill {
                    start_x: 1,
                    end_x: 3,
                    y: 0
                },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 1, y: 1 },
                Region::Boundary { x: 3, y: 1 },
                Region::Fill {
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
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 5.0),
            V2::new(5.0, 0.0),
        ])
        .expect("triangle");

        let regions = RegionList::from(triangle);

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 4, y: 0 },
                Region::Fill {
                    start_x: 1,
                    end_x: 4,
                    y: 0,
                },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 3, y: 1 },
                Region::Fill {
                    start_x: 1,
                    end_x: 3,
                    y: 1,
                },
                Region::Boundary { x: 0, y: 2 },
                Region::Boundary { x: 2, y: 2 },
                Region::Fill {
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
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 3.0),
            V2::new(4.0, 3.0),
            V2::new(2.0, 0.0),
        ])
        .expect("triangle");

        let regions = RegionList::from(triangle);

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
                Region::Fill {
                    start_x: 1,
                    end_x: 3,
                    y: 2,
                },
            ]
        );
    }

    #[test]
    fn quadrilateral_regions() {
        let quad = Polygon::try_from(vec![
            V2::new(3.0, 2.0),
            V2::new(6.0, 4.0),
            V2::new(4.0, 7.0),
            V2::new(1.0, 5.0),
        ])
        .expect("quad");

        let regions = RegionList::from(quad);

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
                Region::Fill {
                    start_x: 3,
                    end_x: 4,
                    y: 3
                },
                Region::Boundary { x: 5, y: 3 },
                Region::Boundary { x: 1, y: 4 },
                Region::Boundary { x: 5, y: 4 },
                Region::Fill {
                    start_x: 2,
                    end_x: 5,
                    y: 4
                },
                Region::Boundary { x: 1, y: 5 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 4, y: 5 },
                Region::Fill {
                    start_x: 3,
                    end_x: 4,
                    y: 5
                },
                Region::Boundary { x: 5, y: 5 },
                Region::Boundary { x: 2, y: 6 },
                Region::Boundary { x: 3, y: 6 },
                Region::Boundary { x: 4, y: 6 },
                Region::Boundary { x: 3, y: 7 }
            ]
        );
    }

    #[test]
    fn irregular_regions() {
        let irregular = Polygon::try_from(vec![
            V2::new(6.18, 5.22),
            V2::new(5.06, 1.07),
            V2::new(2.33, 2.75),
            V2::new(1.69, 6.31),
        ])
        .expect("irregular");

        let regions = RegionList::from(irregular);

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
                Region::Fill {
                    start_x: 4,
                    end_x: 5,
                    y: 2
                },
                Region::Boundary { x: 2, y: 3 },
                Region::Boundary { x: 5, y: 3 },
                Region::Fill {
                    start_x: 3,
                    end_x: 5,
                    y: 3
                },
                Region::Boundary { x: 1, y: 4 },
                Region::Boundary { x: 2, y: 4 },
                Region::Boundary { x: 5, y: 4 },
                Region::Fill {
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
        let irregular = Polygon::try_from(vec![
            V2::new(8.83, 7.46),
            V2::new(7.23, 1.53),
            V2::new(3.33, 3.93),
            V2::new(2.42, 9.02),
        ])
        .expect("irregular");

        let regions = RegionList::from(irregular);

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
                Region::Fill {
                    start_x: 5,
                    end_x: 7,
                    y: 3
                },
                Region::Boundary { x: 3, y: 4 },
                Region::Boundary { x: 7, y: 4 },
                Region::Fill {
                    start_x: 4,
                    end_x: 7,
                    y: 4
                },
                Region::Boundary { x: 8, y: 4 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 3, y: 5 },
                Region::Boundary { x: 8, y: 5 },
                Region::Fill {
                    start_x: 4,
                    end_x: 8,
                    y: 5
                },
                Region::Boundary { x: 2, y: 6 },
                Region::Boundary { x: 8, y: 6 },
                Region::Fill {
                    start_x: 3,
                    end_x: 8,
                    y: 6
                },
                Region::Boundary { x: 2, y: 7 },
                Region::Boundary { x: 6, y: 7 },
                Region::Fill {
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
}
