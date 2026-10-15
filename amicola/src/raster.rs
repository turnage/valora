//! Streaming scanline rasterization.

use crate::{
    edge::Edge,
    sampling::{samples, Sample, SampleDepth},
};
use std::{collections::VecDeque, ops::Range};

/// The rule deciding which regions a path encloses.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum FillRule {
    /// A point is enclosed if the path winds around it any nonzero number of
    /// times.
    #[default]
    NonZero,
    /// A point is enclosed if the path winds around it an odd number of times.
    EvenOdd,
}

impl FillRule {
    fn encloses(self, winding: i32) -> bool {
        match self {
            FillRule::NonZero => winding != 0,
            FillRule::EvenOdd => winding % 2 != 0,
        }
    }
}

/// A command to shade a pixel.
#[derive(Clone, Debug, PartialEq)]
pub enum ShadeCommand {
    /// A command to shade a pixel at the boundary of path's raster area.
    /// Coverage is the pixel's opacity: the fraction of the pixel covered by
    /// the raster area, which is always greater than zero.
    Boundary { x: isize, y: isize, coverage: f32 },
    /// A command to shade a span of the framebuffer. Spans are completely
    /// covered in the raster area, so their opacity is 1.0.
    Span { x: Range<isize>, y: isize },
}

#[derive(Debug, Clone, Copy)]
struct Crossing {
    x: f64,
    /// The winding number of points just to the right of this crossing.
    winding: i32,
}

/// A stream of [`ShadeCommand`]s for a filled or stroked path.
///
/// Commands are produced one pixel row at a time, top to bottom, and are
/// ordered left to right within a row. Each pixel in the raster bounds is
/// shaded at most once.
pub struct Raster {
    /// Edges not yet reached by the scanline, sorted by `top`.
    edges: Vec<Edge>,
    next_edge: usize,
    /// Edges that reach into the current row.
    active: Vec<Edge>,
    row: i64,
    end_row: i64,
    width: i64,
    fill_rule: FillRule,
    samples: Vec<Sample>,
    /// Crossings of each sample's scanline, sorted by x.
    crossings: Vec<Vec<Crossing>>,
    /// Pixel ranges in the current row which edges pass through.
    boundaries: Vec<Range<i64>>,
    pending: VecDeque<ShadeCommand>,
}

impl Raster {
    pub(crate) fn new(
        mut edges: Vec<Edge>,
        fill_rule: FillRule,
        sample_depth: SampleDepth,
        width: u32,
        height: u32,
    ) -> Self {
        let width = width as i64;
        // Edges right of the raster bounds cannot change the winding number of
        // any sample within them.
        edges.retain(|e| e.x_min < width as f64);
        edges.sort_by(|a, b| a.top.total_cmp(&b.top));

        let first_row = edges.first().map_or(0, |e| e.top.floor() as i64).max(0);
        let end_row = edges
            .iter()
            .map(|e| e.bottom.floor() as i64 + 1)
            .max()
            .unwrap_or(0)
            .min(height as i64);
        let samples = samples(sample_depth);

        Self {
            edges,
            next_edge: 0,
            active: vec![],
            row: first_row,
            end_row,
            width,
            fill_rule,
            crossings: vec![vec![]; samples.len()],
            samples,
            boundaries: vec![],
            pending: VecDeque::new(),
        }
    }

    /// Shades the next row that any edge reaches. Returns false when there are
    /// no rows left.
    fn advance(&mut self) -> bool {
        while self.row < self.end_row {
            let top = self.row as f64;
            let bottom = top + 1.;

            self.active.retain(|e| e.reaches(top));
            while let Some(edge) = self.edges.get(self.next_edge).filter(|e| e.top < bottom) {
                if edge.reaches(top) {
                    self.active.push(*edge);
                }
                self.next_edge += 1;
            }

            if self.active.is_empty() {
                match self.edges.get(self.next_edge) {
                    Some(edge) => self.row = (edge.top.floor() as i64).max(self.row + 1),
                    None => self.row = self.end_row,
                }
                continue;
            }

            self.shade_row();
            self.row += 1;
            return true;
        }

        false
    }

    fn shade_row(&mut self) {
        let top = self.row as f64;
        let bottom = top + 1.;

        // A segment within a one pixel tall row passes through every pixel
        // between its leftmost and rightmost points, and no others.
        self.boundaries.clear();
        self.boundaries.extend(self.active.iter().map(|e| {
            let (left, right) = e.x_extent_within(top, bottom);
            (left.floor() as i64)..(right.floor() as i64 + 1)
        }));
        self.boundaries.sort_by_key(|r| r.start);
        merge_ranges(&mut self.boundaries);

        for (sample, crossings) in self.samples.iter().zip(&mut self.crossings) {
            let y = top + sample.y;
            crossings.clear();
            crossings.extend(
                self.active
                    .iter()
                    .filter(|e| e.crosses(y))
                    .map(|e| Crossing {
                        x: e.x_at(y),
                        winding: e.winding,
                    }),
            );
            crossings.sort_by(|a, b| a.x.total_cmp(&b.x));

            let mut winding = 0;
            for crossing in crossings.iter_mut() {
                winding += crossing.winding;
                crossing.winding = winding;
            }
        }

        let y = self.row as isize;
        let fill_rule = self.fill_rule;
        let enclosed =
            |crossings: &[Crossing], x: f64| fill_rule.encloses(winding_at(crossings, x));
        for (i, boundary) in self.boundaries.iter().enumerate() {
            for x in boundary.start.max(0)..boundary.end.min(self.width) {
                let covered = self
                    .samples
                    .iter()
                    .zip(&self.crossings)
                    .filter(|(sample, crossings)| enclosed(crossings, x as f64 + sample.x))
                    .count();
                if covered > 0 {
                    self.pending.push_back(ShadeCommand::Boundary {
                        x: x as isize,
                        y,
                        coverage: covered as f32 / self.samples.len() as f32,
                    });
                }
            }

            // No edge passes through the pixels between boundaries, so one
            // sample decides them all.
            let gap = boundary.end
                ..self
                    .boundaries
                    .get(i + 1)
                    .map_or(self.width, |next| next.start);
            let visible = gap.start.max(0)..gap.end.min(self.width);
            if !visible.is_empty() && enclosed(&self.crossings[0], gap.start as f64 + 0.5) {
                self.pending.push_back(ShadeCommand::Span {
                    x: (visible.start as isize)..(visible.end as isize),
                    y,
                });
            }
        }
    }
}

impl Iterator for Raster {
    type Item = ShadeCommand;

    fn next(&mut self) -> Option<ShadeCommand> {
        loop {
            if let Some(command) = self.pending.pop_front() {
                return Some(command);
            }
            if !self.advance() {
                return None;
            }
        }
    }
}

/// The winding number at `x` along a scanline with the given crossings.
fn winding_at(crossings: &[Crossing], x: f64) -> i32 {
    match crossings.partition_point(|c| c.x < x) {
        0 => 0,
        i => crossings[i - 1].winding,
    }
}

/// Merges overlapping and adjacent ranges in a list sorted by start.
fn merge_ranges(ranges: &mut Vec<Range<i64>>) {
    let mut merged = 0;
    for i in 1..ranges.len() {
        if ranges[i].start <= ranges[merged].end {
            ranges[merged].end = ranges[merged].end.max(ranges[i].end);
        } else {
            merged += 1;
            ranges[merged] = ranges[i].clone();
        }
    }
    ranges.truncate(merged + 1);
}

/// Expands shade commands into a row-major opacity map, asserting that no
/// pixel is shaded twice.
#[cfg(test)]
pub(crate) fn opacity_map(
    commands: impl Iterator<Item = ShadeCommand>,
    width: u32,
    height: u32,
) -> Vec<f32> {
    let mut map = vec![0.; (width * height) as usize];
    let mut shade = |x: isize, y: isize, opacity: f32| {
        assert!(x >= 0 && x < width as isize && y >= 0 && y < height as isize);
        let pixel = &mut map[y as usize * width as usize + x as usize];
        assert_eq!(*pixel, 0., "pixel ({}, {}) shaded twice", x, y);
        *pixel = opacity;
    };
    for command in commands {
        match command {
            ShadeCommand::Boundary { x, y, coverage } => shade(x, y, coverage),
            ShadeCommand::Span { x, y } => x.for_each(|x| shade(x, y, 1.)),
        }
    }
    map
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::edge::path_edges;
    use kurbo::{BezPath, Point};

    const DEPTHS: [SampleDepth; 3] = [
        SampleDepth::Single,
        SampleDepth::Super4,
        SampleDepth::Super16,
    ];

    /// Shades every pixel by testing every sample against every edge.
    fn reference_opacity_map(
        edges: &[Edge],
        fill_rule: FillRule,
        depth: SampleDepth,
        width: u32,
        height: u32,
    ) -> Vec<f32> {
        let samples = samples(depth);
        let mut map = vec![];
        for y in 0..height {
            for x in 0..width {
                let covered = samples
                    .iter()
                    .filter(|sample| {
                        let (sx, sy) = (x as f64 + sample.x, y as f64 + sample.y);
                        let winding = edges
                            .iter()
                            .filter(|e| e.crosses(sy) && e.x_at(sy) < sx)
                            .map(|e| e.winding)
                            .sum();
                        fill_rule.encloses(winding)
                    })
                    .count();
                map.push(covered as f32 / samples.len() as f32);
            }
        }
        map
    }

    /// A xorshift generator, so tests are deterministic.
    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> f64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            (self.0 >> 11) as f64 / (1u64 << 53) as f64
        }

        fn range(&mut self, range: Range<f64>) -> f64 {
            range.start + self.next() * (range.end - range.start)
        }
    }

    fn random_path(rng: &mut Rng, subpaths: usize) -> BezPath {
        let mut path = BezPath::new();
        for _ in 0..subpaths {
            let vertices = 3 + (rng.next() * 10.) as usize;
            path.move_to((rng.range(-8.0..40.), rng.range(-8.0..40.)));
            for _ in 1..vertices {
                path.line_to((rng.range(-8.0..40.), rng.range(-8.0..40.)));
            }
            path.close_path();
        }
        path
    }

    #[test]
    fn matches_reference_on_random_polygons() {
        let (width, height) = (32, 30);
        let mut rng = Rng(0x9e3779b97f4a7c15);
        for case in 0..200 {
            let edges = path_edges(random_path(&mut rng, 1 + case % 3), 0.1);
            for fill_rule in [FillRule::NonZero, FillRule::EvenOdd] {
                for depth in DEPTHS {
                    let raster = Raster::new(edges.clone(), fill_rule, depth, width, height);
                    assert_eq!(
                        opacity_map(raster, width, height),
                        reference_opacity_map(&edges, fill_rule, depth, width, height),
                        "case {} {:?} {:?}",
                        case,
                        fill_rule,
                        depth
                    );
                }
            }
        }
    }

    #[test]
    fn matches_reference_on_grid_aligned_polygons() {
        // Vertices and edges on pixel boundaries exercise the tie breaking
        // rules.
        let (width, height) = (16, 16);
        let mut rng = Rng(0x2545f4914f6cdd1d);
        for case in 0..200 {
            let mut path = BezPath::new();
            let mut point = || {
                Point::new(
                    (rng.next() * 20.).floor() - 2.,
                    (rng.next() * 20.).floor() - 2.,
                )
            };
            path.move_to(point());
            for _ in 0..(2 + case % 6) {
                path.line_to(point());
            }
            path.close_path();
            let edges = path_edges(path, 0.1);

            for fill_rule in [FillRule::NonZero, FillRule::EvenOdd] {
                for depth in DEPTHS {
                    let raster = Raster::new(edges.clone(), fill_rule, depth, width, height);
                    assert_eq!(
                        opacity_map(raster, width, height),
                        reference_opacity_map(&edges, fill_rule, depth, width, height),
                        "case {} {:?} {:?}",
                        case,
                        fill_rule,
                        depth
                    );
                }
            }
        }
    }

    #[test]
    fn commands_are_row_major() {
        let mut rng = Rng(7);
        let edges = path_edges(random_path(&mut rng, 3), 0.1);
        let raster = Raster::new(edges, FillRule::NonZero, SampleDepth::Super4, 32, 32);

        let starts: Vec<(isize, isize)> = raster
            .map(|command| match command {
                ShadeCommand::Boundary { x, y, .. } => (y, x),
                ShadeCommand::Span { x, y } => (y, x.start),
            })
            .collect();
        let mut sorted = starts.clone();
        sorted.sort();
        assert!(!starts.is_empty());
        assert_eq!(starts, sorted);
    }

    #[test]
    fn empty() {
        let raster = Raster::new(vec![], FillRule::NonZero, SampleDepth::Single, 10, 10);
        assert_eq!(raster.count(), 0);
    }

    #[test]
    fn merge() {
        let mut ranges = vec![0..2, 1..3, 3..4, 6..7, 6..8, 9..10];
        merge_ranges(&mut ranges);
        assert_eq!(ranges, vec![0..4, 6..8, 9..10]);
    }
}
