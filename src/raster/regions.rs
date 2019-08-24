//! Raster region search and enumeration.

use super::{grid_lines::*, path::*};
use crate::geo::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Region {
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

impl IntoIterator for Region {
    type IntoIter = ShadeCommandIter;
    type Item = ShadeCommand;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            Region::Boundary { x, y } => ShadeCommandIter {
                x,
                y,
                end_x: x + 1,
                super_sample: true,
            },
            Region::Fill { start_x, end_x, y } => ShadeCommandIter {
                x: start_x,
                y,
                end_x,
                super_sample: false,
            },
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ShadeCommand {
    pub x: usize,
    pub y: usize,
    pub super_sample: bool,
}

pub struct ShadeCommandIter {
    x: usize,
    y: usize,
    end_x: usize,
    super_sample: bool,
}

impl Iterator for ShadeCommandIter {
    type Item = ShadeCommand;

    fn next(&mut self) -> Option<Self::Item> {
        if self.x < self.end_x {
            self.x += 1;
            Some(ShadeCommand {
                x: self.x - 1,
                y: self.y,
                super_sample: self.super_sample,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct RegionList {
    scan_lines: Vec<Vec<usize>>,
}

impl RegionList {
    pub fn new() -> Self {
        Self { scan_lines: vec![] }
    }

    pub fn push(&mut self, poly: Polygon) {
        poly.edges()
            .map(MonotonicSegment::try_from)
            .filter_map(Result::ok)
            .for_each(|segment| {
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
                        self.mark_intersection(horizontal_line, self.scan_column(x));
                    }
                }
            });
    }

    pub fn regions(mut self) -> impl Iterator<Item = Region> {
        self.scan_lines
            .into_iter()
            .enumerate()
            .flat_map(|(y, mut scan_line)| {
                scan_line.sort_unstable();
                RegionIter {
                    scan_line,
                    y,
                    i: 0,
                    last_on_row: None,
                    queue: None,
                }
            })
    }

    fn mark_intersection(&mut self, row: usize, column: usize) {
        self.reserve_up_to(row);
        self.scan_lines[row].push(column);
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

struct RegionIter {
    /// Scan line assumed to have intersections sorted by left to right.
    scan_line: Vec<usize>,
    y: usize,
    i: usize,
    last_on_row: Option<usize>,
    queue: Option<Region>,
}

impl Iterator for RegionIter {
    type Item = Region;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(queue) = self.queue.take() {
            return Some(queue);
        }

        let next_boundary = self.scan_line.get(self.i)?;
        if let Some(last_on_row) = self.last_on_row.take() {
            if *next_boundary > last_on_row + 1 {
                self.queue = Some(Region::Fill {
                    start_x: last_on_row + 1,
                    end_x: *next_boundary,
                    y: self.y,
                });
            }
        } else {
            self.last_on_row = Some(*next_boundary);
        }

        self.i += 1;
        Some(Region::Boundary {
            x: *next_boundary,
            y: self.y,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::*;

    #[test]
    fn triangle_regions() {
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 2.0),
            V2::new(2.0, 0.0),
        ])
        .expect("triangle");

        let mut regions = RegionList::new();
        regions.push(triangle);

        println!("Regions: {:#?}", regions);

        assert_eq!(
            regions.regions().collect::<Vec<Region>>(),
            vec![
                Region::Boundary { x: 0, y: 0 },
                Region::Boundary { x: 2, y: 0 },
                Region::Fill {
                    start_x: 1,
                    end_x: 2,
                    y: 0
                },
                Region::Boundary { x: 0, y: 1 },
                Region::Boundary { x: 1, y: 1 },
                Region::Boundary { x: 0, y: 2 },
                Region::Boundary { x: 0, y: 2 }
            ]
        );
    }
}
