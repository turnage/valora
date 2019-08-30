//! Raster region search and enumeration.

use super::{grid_lines::*, path::*, sampling::*};
use crate::amicola::geo::*;
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
                intersections.push(Intersection {
                    location: V2::new(x, y),
                    t,
                    segment_id,
                });
            };

            for horizontal_line in iter.horizontal() {
                if let Some(intersection) = segment.sample_y(horizontal_line as f64) {
                    let floor = intersection.axis.floor();
                    if floor == intersection.axis {
                        excluded_verticals.insert(floor as usize);
                    }
                    add_intersection(intersection.axis, horizontal_line as f64, intersection.t);
                }
            }

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

            intersections.sort_unstable_by(|a, b| a.t.partial_cmp(&b.t).unwrap());

            for (a, b) in intersections
                .windows(2)
                .map(|w| (w[0].location, w[1].location))
            {
                let midpoint = (a + b) / 2.0;
                let (x, y) = (midpoint.x.floor() as usize, midpoint.y.floor() as usize);
                let path_hit = PathHit { x, segment_id };
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

        if self
            .last_on_row
            .as_ref()
            .map(|b| b.segment_id == next_boundary.segment_id)
            .unwrap_or(false)
        {
            // Each segment can only increment the winding number once.
            self.last_on_row.replace(*next_boundary);
        } else if let Some(last_on_row) = self.last_on_row.take() {
            if next_boundary.x > last_on_row.x + 1 {
                self.queue = Some(Region::Fill {
                    start_x: last_on_row.x + 1,
                    end_x: next_boundary.x,
                    y: self.y,
                });
            }
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
    use pretty_assertions::assert_eq;
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
}
