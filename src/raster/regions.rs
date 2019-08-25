//! Raster region search and enumeration.

use super::{grid_lines::*, path::*, sampling::*};
use crate::geo::*;

#[derive(Copy, Clone, Debug, PartialEq)]
enum Region {
    Boundary {
        x: usize,
        y: usize,
        // path index
        path: usize,
    },
    Fill {
        start_x: usize,
        end_x: usize,
        y: usize,
        // path index
        path: usize,
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

#[derive(Debug, Copy, Clone)]
struct PathHit {
    x: usize,
    path: usize,
}

#[derive(Debug, Default)]
pub struct RegionList {
    paths: Vec<Vec<MonotonicSegment>>,
    /// Scan lines assumed to have intersections sorted by left to right.
    scan_lines: Vec<Vec<PathHit>>,
}

impl std::iter::FromIterator<Polygon> for RegionList {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Polygon>,
    {
        let mut list = RegionList::default();

        for polygon in iter {
            list.push(polygon);
        }

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

        let path_idx = self.paths.len();

        for segment in &path {
            let bounds = segment.bounds();
            println!("Bounds: {:?}", bounds);
            self.reserve_up_to(bounds.top.ceil() as usize);

            let iter = GridLinesIter::Bounds(bounds);
            for (i, horizontal_line) in iter.horizontal().enumerate() {
                if let Some(x) = segment.sample(horizontal_line as f64).or_else(|| {
                    let x = if i == 0 { bounds.bottom } else { bounds.top };
                    let s = segment.sample(x);
                    println!("Sampled: {:?}", s);
                    s
                }) {
                    println!("Sampling at {:?} -> {:?}", horizontal_line, x);
                    self.mark_intersection(
                        horizontal_line,
                        PathHit {
                            x: self.scan_column(x),
                            path: path_idx,
                        },
                    );
                }
            }
        }

        self.paths.push(path);
    }

    pub fn shade_commands<'a>(&'a self) -> impl Iterator<Item = ShadeCommand> + 'a {
        self.regions().flat_map(move |region| match region {
            Region::Boundary { x, y, path } => ShadeCommandIter {
                x,
                y,
                end_x: x + 1,
                coverage: coverage(
                    V2::new(x as f64, y as f64),
                    SampleDepth::Super4,
                    &self.paths[path],
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
            println!("additional: {:?}", additional);
            println!("upper bound: {:?}", upper_bound);
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
    last_on_row: Option<usize>,
    queue: Option<Region>,
}

impl<'a> Iterator for RegionIter<'a> {
    type Item = Region;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(queue) = self.queue.take() {
            return Some(queue);
        }

        let next_boundary = self.region_list.scan_lines[self.y].get(self.i)?.x;
        if let Some(last_on_row) = self.last_on_row.take() {
            if next_boundary > last_on_row + 1 {
                self.queue = Some(Region::Fill {
                    start_x: last_on_row + 1,
                    end_x: next_boundary,
                    y: self.y,
                    path: 0, // TODO
                });
            }
        } else {
            self.last_on_row = Some(next_boundary);
        }

        self.i += 1;
        Some(Region::Boundary {
            x: next_boundary,
            y: self.y,
            path: 0, // TODO
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::*;
    use std::iter::*;

    #[test]
    fn triangle_regions() {
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 2.0),
            V2::new(2.0, 0.0),
        ])
        .expect("triangle");

        let regions = RegionList::from_iter(vec![triangle]);

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary {
                    x: 0,
                    y: 0,
                    path: 0
                },
                Region::Boundary {
                    x: 2,
                    y: 0,
                    path: 0
                },
                Region::Fill {
                    start_x: 1,
                    end_x: 2,
                    y: 0,
                    path: 0
                },
                Region::Boundary {
                    x: 0,
                    y: 1,
                    path: 0
                },
                Region::Boundary {
                    x: 1,
                    y: 1,
                    path: 0
                },
                Region::Boundary {
                    x: 0,
                    y: 2,
                    path: 0
                },
                Region::Boundary {
                    x: 0,
                    y: 2,
                    path: 0
                }
            ]
        );
    }
}
