//! Super sampling patterns.

use std::fmt::Debug;
use structopt::StructOpt;

/// Super sampling depths for the hammersley pattern.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, StructOpt)]
pub enum SampleDepth {
    #[default]
    Single,
    Super4,
    Super8,
    Super16,
    Super32,
    Super64,
}

impl From<SampleDepth> for u64 {
    fn from(depth: SampleDepth) -> u64 {
        match depth {
            SampleDepth::Single => 1,
            SampleDepth::Super4 => 4,
            SampleDepth::Super8 => 8,
            SampleDepth::Super16 => 16,
            SampleDepth::Super32 => 32,
            SampleDepth::Super64 => 64,
        }
    }
}

/// A sample position, as an offset from the top left corner of a pixel.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Sample {
    pub x: f64,
    pub y: f64,
}

/// Sample positions within a pixel, following a hammersley pattern shifted to
/// the center of each stratum, so that a single sample lands in the center of
/// the pixel.
pub(crate) fn samples(depth: SampleDepth) -> Vec<Sample> {
    let n: u64 = depth.into();
    let half_stratum = 0.5 / n as f64;
    (0..n)
        .map(|i| Sample {
            x: i as f64 / n as f64 + half_stratum,
            y: van_der_corput(i) + half_stratum,
        })
        .collect()
}

// Van der Corput sequence base 2.
fn van_der_corput(word: u64) -> f64 {
    word.reverse_bits() as f64 / 2f64.powi(64)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_sample_is_centered() {
        assert_eq!(
            samples(SampleDepth::Single),
            vec![Sample { x: 0.5, y: 0.5 }]
        );
    }

    #[test]
    fn hammersley_generation() {
        assert_eq!(
            samples(SampleDepth::Super4),
            vec![
                Sample { x: 0.125, y: 0.125 },
                Sample { x: 0.375, y: 0.625 },
                Sample { x: 0.625, y: 0.375 },
                Sample { x: 0.875, y: 0.875 },
            ]
        );
    }

    #[test]
    fn samples_are_within_pixel() {
        for sample in samples(SampleDepth::Super64) {
            assert!((0.0..1.0).contains(&sample.x), "{:?}", sample);
            assert!((0.0..1.0).contains(&sample.y), "{:?}", sample);
        }
    }
}
