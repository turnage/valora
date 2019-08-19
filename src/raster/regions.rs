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
                    //println!("Sampling {:?} at {:?}", segment, horizontal_line);
                    if let Some(y) = segment.sample(horizontal_line as f64).or_else(|| {
                        let x = if i == 0 { bounds.left } else { bounds.right };
                        let s = segment.sample(x);
                        println!("Sampled: {:?}", s);
                        s
                    }) {
                        self.mark_intersection(self.scan_line(y), horizontal_line);
                    }
                }
            });
    }

    pub fn regions(mut self) -> impl Iterator<Item = Region> {
        for scan_line in self.scan_lines.iter_mut() {
            scan_line.sort_unstable();
        }

        RegionIter {
            scan_lines: self.scan_lines,
            row: 0,
            i: 0,
            last_on_row: None,
            queue: None,
        }
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

    fn scan_line(&self, y: f64) -> usize {
        y.floor() as usize
    }
}

struct RegionIter {
    /// Scan lines where each line is assumed to have intersections sorted by left to right.
    scan_lines: Vec<Vec<usize>>,
    row: usize,
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

        let scan_line = self.scan_lines.get(self.row)?;
        let next_boundary = scan_line.get(self.i)?;
        if let Some(last_on_row) = self.last_on_row.take() {
            if next_boundary - 1 > last_on_row {
                self.queue = Some(Region::Fill {
                    start_x: last_on_row + 1,
                    end_x: *next_boundary,
                    y: self.row,
                });
            }
        }

        self.last_on_row = Some(*next_boundary);
        let next = Some(Region::Boundary {
            x: *next_boundary,
            y: self.row,
        });

        if self.i > self.scan_lines[self.row].len() {
            self.i = 0;
            self.row += 1;
        } else {
            self.i += 1;
        }

        next
    }
}

#[cfg(test)]
mod test {}
