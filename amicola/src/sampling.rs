//! Super sampling patterns.

use crate::V2;
use lyon_geom::LineSegment;
use structopt::StructOpt;

/// Super sampling depths for the hammersley pattern.
#[derive(Debug, Clone, Copy, PartialEq, StructOpt)]
pub enum SampleDepth {
    Single,
    Super4,
    Super8,
    Super16,
    Super32,
    Super64,
}

impl Into<u64> for SampleDepth {
    fn into(self) -> u64 {
        match self {
            SampleDepth::Single => 1,
            SampleDepth::Super4 => 4,
            SampleDepth::Super8 => 8,
            SampleDepth::Super16 => 16,
            SampleDepth::Super32 => 32,
            SampleDepth::Super64 => 64,
        }
    }
}

pub fn coverage(
    offset: V2,
    depth: SampleDepth,
    path: impl Iterator<Item = LineSegment<f32>> + Clone,
) -> f32 {
    // TODO: Sample vertically as well?
    let mut hits = 0;
    for command in hammersley(depth).map(|mut cmd| {
        cmd.x += offset.x;
        cmd.y += offset.y;
        cmd
    }) {
        let mut pass_count = 0;
        for segment in path.clone() {
            if let Some(y) = segment.vertical_line_intersection(command.x).map(|p| p.y) {
                if y <= command.y {
                    pass_count += 1;
                }
            }
        }
        if pass_count % 2 != 0 {
            hits += 1;
        }
    }

    let total_samples: u64 = depth.into();
    hits as f32 / total_samples as f32
}

fn hammersley(depth: SampleDepth) -> impl Iterator<Item = V2> {
    let n: u64 = depth.into();
    (0..n).map(move |i| V2::new(i as f32 / n as f32, van_der_corput(i) as f32))
}

// Van der Corput sequence base 2.
fn van_der_corput(word: u64) -> f32 {
    (0..63)
        .map(|i| (i, 1 << i))
        .filter(|(_, bit)| word & bit != 0)
        .map(|(i, _)| 1.0 / 2.0f32.powi((i + 1) as i32))
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn hammersely_generation() {
        assert_eq!(
            hammersley(SampleDepth::Super4).collect::<Vec<V2>>(),
            vec![
                V2::new(0.0, 0.0),
                V2::new(0.25, 0.5),
                V2::new(0.5, 0.25),
                V2::new(0.75, 0.75),
            ]
        );
    }
}
