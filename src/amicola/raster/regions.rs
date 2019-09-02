//! Raster region search and enumeration.

use super::{grid_lines::*, path::*, sampling::*};
use crate::amicola::geo::*;
use log::trace;
use std::collections::HashSet;

#[derive(Copy, Clone, Debug, PartialEq)]
enum Region {
    Boundary {
        x: usize,
        y: usize,
    },
    Fill {
        start_x: usize,
        end_x: usize,
        y: usize,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ShadeCommand {
    pub x: usize,
    pub y: usize,
    pub coverage: f64,
}

pub struct ShadeCommandIter {
    x: usize,
    y: usize,
    end_x: usize,
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

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct PathHit {
    x: usize,
    segment_id: usize,
}

#[derive(Debug, Default)]
pub struct RegionList {
    /// Scan lines assumed to have intersections sorted by left to right.
    scan_lines: Vec<Vec<PathHit>>,
    path: Vec<MonotonicSegment>,
}

impl From<Polygon> for RegionList {
    fn from(poly: Polygon) -> Self {
        let mut list = RegionList::default();

        list.push(poly);
        list.scan_lines
            .iter_mut()
            .for_each(|scan_line| scan_line.sort_unstable_by_key(|p| p.x));

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
        let mut seen_hits = HashSet::new();

        for (segment_id, segment) in path.iter().enumerate() {
            let bounds = segment.bounds();
            self.reserve_up_to(bounds.top.ceil() as usize);

            println!("Testing segment: {:#?}", segment);

            #[derive(Debug)]
            pub struct Intersection {
                location: V2,
                t: f64,
                segment_id: usize,
            }

            let mut intersections = vec![];
            let mut excluded_verticals = HashSet::new();
            let mut has_t1 = false;
            let mut has_t0 = false;
            let iter = GridLinesIter::Bounds(bounds);
            let mut add_intersection = |x: f64, y: f64, t: f64| {
                has_t0 |= t.abs() == 0.0;
                has_t1 |= t == 1.0;
                println!("Pushing intersection at ({:?}, {:?})", x, y);
                intersections.push(Intersection {
                    location: V2::new(x, y),
                    t,
                    segment_id,
                });
            };

            for horizontal_line in iter.horizontal() {
                println!("Testing against horizonal {:?}: ", horizontal_line);
                if let Some(intersection) = segment.sample_y(horizontal_line as f64) {
                    println!("\tIntersection found: {:?}", intersection);
                    let floor = intersection.axis.floor();
                    if floor == intersection.axis {
                        excluded_verticals.insert(floor as usize);
                    }
                    add_intersection(intersection.axis, horizontal_line as f64, intersection.t);
                } else {
                    println!("\tNo intersection with vertical line found.");
                }
            }

            println!(
                "Now testing vertical lines: excluded verticals: {:?}",
                excluded_verticals
            );

            for vertical_line in iter
                .vertical()
                .filter(|v| excluded_verticals.get(&v).is_none())
            {
                if let Some(intersection) = segment.sample_x(vertical_line as f64) {
                    add_intersection(vertical_line as f64, intersection.axis, intersection.t);
                }
            }

            let (start, end) = segment.bookends();
            if !has_t0 {
                intersections.push(Intersection {
                    location: start,
                    t: 0.0,
                    segment_id,
                });
            }
            if !has_t1 {
                intersections.push(Intersection {
                    location: end,
                    t: 1.0,
                    segment_id,
                });
            }

            intersections.sort_by(|a, b| a.t.partial_cmp(&b.t).unwrap());

            for (ref ai, ref bi) in intersections.as_slice().windows(2).map(|w| (&w[0], &w[1])) {
                let (a, b) = (ai.location, bi.location);
                let midpoint = (a + b) / 2.0;
                let (x, y) = (midpoint.x.floor() as usize, midpoint.y.floor() as usize);
                let path_hit = PathHit { x, segment_id };
                println!(
                    "Intersection ({:?}-{:?}) becomes path hit ({:#?} at y {:?})",
                    ai, bi, path_hit, y
                );
                if seen_hits.get(&(y, path_hit)).is_none() {
                    self.mark_intersection(y, path_hit);
                    seen_hits.insert((y, path_hit));
                }
            }
        }

        self.path = path;
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
                    &self.path,
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
        (0..(self.scan_lines.len())).flat_map(move |y| RegionIter {
            region_list: &self,
            y,
            i: 0,
            last_on_row: None,
            queue: None,
        })
    }

    fn mark_intersection(&mut self, row: usize, hit: PathHit) {
        self.reserve_up_to(row);
        self.scan_lines[row].push(hit);
    }

    fn reserve_up_to(&mut self, upper_bound: usize) {
        let rows_allocated = self.scan_lines.len();
        if upper_bound + 1 > rows_allocated {
            let additional = 1 + upper_bound - rows_allocated;
            self.scan_lines.append(&mut vec![vec![]; additional]);
        }
    }

    fn scan_column(&self, x: f64) -> usize {
        x.floor() as usize
    }
}

struct RegionIter<'a> {
    region_list: &'a RegionList,
    y: usize,
    i: usize,
    last_on_row: Option<PathHit>,
    queue: Option<Region>,
}

impl<'a> Iterator for RegionIter<'a> {
    type Item = Region;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(queue) = self.queue.take() {
            return Some(queue);
        }

        let next_boundary = self.region_list.scan_lines[self.y].get(self.i)?;
        let difference = |a, b| {
            if a > b {
                a - b
            } else {
                b - a
            }
        };

        if self
            .last_on_row
            .as_ref()
            .map(|b| {
                // Each segment can only increment the winding number once.
                // There must also be a gap to fill.
                b.segment_id == next_boundary.segment_id || difference(b.x, next_boundary.x) <= 1
            })
            .unwrap_or(false)
        {
            self.last_on_row.replace(*next_boundary);
        } else if let Some(last_on_row) = self.last_on_row.take() {
            self.queue = Some(Region::Fill {
                start_x: last_on_row.x + 1,
                end_x: next_boundary.x,
                y: self.y,
            });
        } else {
            self.last_on_row = Some(*next_boundary);
        }

        self.i += 1;
        Some(Region::Boundary {
            x: next_boundary.x,
            y: self.y,
        })
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
                    y: 3,
                },
                Region::Boundary { x: 5, y: 3 },
                Region::Boundary { x: 1, y: 4 },
                Region::Boundary { x: 5, y: 4 },
                Region::Fill {
                    start_x: 2,
                    end_x: 5,
                    y: 4,
                },
                Region::Boundary { x: 1, y: 5 },
                Region::Boundary { x: 2, y: 5 },
                Region::Boundary { x: 4, y: 5 },
                Region::Fill {
                    start_x: 3,
                    end_x: 4,
                    y: 5,
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
                Region::Boundary { x: 5, y: 1 },
                Region::Boundary { x: 2, y: 2 },
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
                Region::Boundary { x: 6, y: 5 },
                Region::Boundary { x: 1, y: 6 },
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
                Region::Boundary { x: 7, y: 1 },
                Region::Boundary { x: 4, y: 2 },
                Region::Boundary { x: 5, y: 2 },
                Region::Boundary { x: 6, y: 2 },
                Region::Boundary { x: 7, y: 2 },
                Region::Boundary { x: 3, y: 3 },
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
                Region::Boundary { x: 8, y: 7 },
                Region::Boundary { x: 2, y: 8 },
                Region::Boundary { x: 2, y: 8 },
                Region::Boundary { x: 3, y: 8 },
                Region::Boundary { x: 4, y: 8 },
                Region::Boundary { x: 5, y: 8 },
                Region::Boundary { x: 6, y: 8 },
                Region::Boundary { x: 2, y: 9 },
                Region::Boundary { x: 2, y: 9 }
            ]
        );
    }
}
