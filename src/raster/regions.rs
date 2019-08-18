//! Raster region search and enumeration.

use super::{grid_lines::*, path::*};
use crate::geo::*;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Region {
    Boundary { x: usize, y: usize },
    Fill { start_x: usize, end_x: usize },
}

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
                self.reserve_up_to(bounds.top.ceil() as usize);

                let iter = GridLinesIter::Bounds(bounds);
                for (i, horizontal_line) in iter.horizontal().enumerate() {
                    match segment.sample(horizontal_line as f64) {
                        None => {
                            let x = if i == 0 { bounds.left } else { bounds.right };
                            if let Some(y) = segment.sample(x) {
                                let row = self.scan_line(y);
                                self.scan_lines[row].push(horizontal_line);
                            }
                        }
                        Some(y) => {
                            let row = self.scan_line(y);
                            self.scan_lines[row].push(horizontal_line);
                        }
                    }
                }
            });
    }

    pub fn regions(mut self) {
        for scan_line in self.scan_lines.iter_mut() {
            scan_line.sort_unstable();
        }

        unimplemented!()
    }

    fn reserve_up_to(&mut self, upper_bound: usize) {
        let rows_allocated = self.scan_lines.len();
        if upper_bound > rows_allocated {
            self.scan_lines
                .append(&mut vec![vec![]; upper_bound - rows_allocated]);
        }
    }

    fn scan_line(&self, y: f64) -> usize {
        y.floor() as usize
    }
}

/*
struct RegionIter {
    scan_lines: Vec<Vec<usize>>
}

impl Iterator for RegionIter {
    type Item = Region;
    fn next(&mut self) -> Option<Self::Item> {

    }
}

#[cfg(test)]
mod test {
    #[test]
    fn
}*/
