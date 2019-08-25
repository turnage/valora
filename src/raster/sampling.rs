//! Super sampling patterns.

use super::path::*;
use crate::geo::*;

#[derive(Debug, Clone, Copy, PartialEq)]
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

pub fn coverage<'a>(offset: V2, depth: SampleDepth, path: &[MonotonicSegment]) -> f64 {
    let mut hits = 0;
    for command in hammersley(depth).map(|cmd| cmd + offset) {
        let mut pass_count = 0;
        for segment in path {
            if let Some(x) = segment.sample_y(command.y).map(|i| i.axis) {
                if x <= command.x {
                    pass_count += 1;
                }
            }
        }
        if pass_count % 2 != 0 {
            hits += 1;
        }
    }

    let total_samples: u64 = depth.into();
    hits as f64 / total_samples as f64
}

fn hammersley(depth: SampleDepth) -> impl Iterator<Item = V2> {
    let n: u64 = depth.into();
    (0..n).map(move |i| V2::new(i as f64 / n as f64, van_der_corput(i) as f64))
}

// Van der Corput sequence base 2.
fn van_der_corput(word: u64) -> f64 {
    (0..63)
        .map(|i| (i, 1 << i))
        .filter(|(_, bit)| word & bit != 0)
        .map(|(i, _)| 1.0 / 2.0f64.powi((i + 1) as i32))
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
