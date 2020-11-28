//! Super sampling patterns.

use crate::V2;
use lyon_geom::LineSegment;
use std::fmt::Debug;
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
    path: impl Iterator<Item = LineSegment<f64>> + Clone + Debug,
) -> f64 {
    let total_samples: u64 = depth.into();
    let total_samples: f64 = total_samples as f64;
    hammersley(depth)
        .map(|cmd| cmd + offset.to_vector())
        .filter_map(|command| {
            let path = path.clone();
            let pass_count: usize = path
                .filter_map(|segment| {
                    segment
                        .horizontal_line_intersection(command.y as f64)
                        .filter(|p| p.x <= command.x as f64)
                        .map(|_| 1)
                })
                .sum();

            match pass_count % 2 {
                0 => None,
                _ => Some(1. / total_samples),
            }
        })
        .sum()
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
    use float_eq::assert_float_eq;
    use lyon_path::math::point;

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

    #[test]
    fn antialias() {
        let coverage = coverage(
            V2::new(1., 3.), // offset
            SampleDepth::Super64,
            vec![
                LineSegment {
                    from: point(0., 0.),
                    to: point(0., 5.),
                },
                LineSegment {
                    from: point(0., 5.),
                    to: point(5., 0.),
                },
            ]
            .into_iter(),
        );

        assert_float_eq!(coverage, 0.5, abs <= 0.1);
    }

    #[test]
    fn antialias_large() {
        let coverage = coverage(
            V2::new(343., 656.), // offset
            SampleDepth::Super64,
            vec![
                LineSegment {
                    from: point(0., 0.),
                    to: point(0., 1000.),
                },
                LineSegment {
                    from: point(0., 1000.),
                    to: point(1000., 0.),
                },
            ]
            .into_iter(),
        );

        assert_float_eq!(coverage, 0.5, abs <= 0.1);
    }
}
