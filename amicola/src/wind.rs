use crate::{boundary_spans::*, grid_lines::*, Pixel};
use std::collections::BTreeSet;

/// Processes a sorted iterator of `Hit` and `BoundarySpan` into `BoundarySpan`s
/// tagged with winding numbers.
pub(crate) fn wind_spans(
    mut hits: impl Iterator<Item = Hit>,
    mut boundary_spans: impl Iterator<Item = BoundarySpan>,
) -> impl Iterator<Item = (i32, BoundarySpan)> {
    let mut cached = None;
    let mut wind = 0;
    let mut row = 0;
    boundary_spans.map(move |span| {
        let BoundarySpan { y, xs } = &span;
        if *y != row {
            row = *y;
            wind = 0;
        }
        while let Some(hit) = cached.take().or_else(|| hits.next()) {
            if hit.pixel.y != *y || !xs.contains(&hit.pixel.x) {
                cached = Some(hit);
                break;
            }

            wind += 1;
        }

        (wind, span)
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn row_gap() {
        let hits = vec![
            Hit {
                x: 1.2,
                pixel: Pixel { x: 1, y: 1 },
            },
            Hit {
                x: 4.2,
                pixel: Pixel { x: 4, y: 2 },
            },
        ]
        .into_iter();

        let boundary_spans = vec![
            BoundarySpan { y: 1, xs: 1..2 },
            BoundarySpan { y: 2, xs: 4..5 },
        ]
        .into_iter();

        let windings: Vec<(i32, BoundarySpan)> = wind_spans(hits, boundary_spans).collect();

        assert_eq!(
            windings,
            vec![
                (1, BoundarySpan { y: 1, xs: 1..2 }),
                (1, BoundarySpan { y: 2, xs: 4..5 }),
            ]
        );
    }

    #[test]
    fn gapped_hits() {
        let hits = vec![
            Hit {
                x: 1.2,
                pixel: Pixel { x: 1, y: 1 },
            },
            Hit {
                x: 4.2,
                pixel: Pixel { x: 4, y: 1 },
            },
        ]
        .into_iter();

        let boundary_spans = vec![
            BoundarySpan { y: 1, xs: 1..2 },
            BoundarySpan { y: 1, xs: 4..5 },
        ]
        .into_iter();

        let windings: Vec<(i32, BoundarySpan)> = wind_spans(hits, boundary_spans).collect();

        assert_eq!(
            windings,
            vec![
                (1, BoundarySpan { y: 1, xs: 1..2 }),
                (2, BoundarySpan { y: 1, xs: 4..5 }),
            ]
        );
    }

    #[test]
    fn adjacent_hits() {
        let hits = vec![
            Hit {
                x: 1.2,
                pixel: Pixel { x: 1, y: 1 },
            },
            Hit {
                x: 1.5,
                pixel: Pixel { x: 1, y: 1 },
            },
            Hit {
                x: 4.2,
                pixel: Pixel { x: 4, y: 1 },
            },
        ]
        .into_iter();

        let boundary_spans = vec![
            BoundarySpan { y: 1, xs: 1..2 },
            BoundarySpan { y: 1, xs: 4..5 },
        ]
        .into_iter();

        let windings: Vec<(i32, BoundarySpan)> = wind_spans(hits, boundary_spans).collect();

        assert_eq!(
            windings,
            vec![
                (2, BoundarySpan { y: 1, xs: 1..2 }),
                (3, BoundarySpan { y: 1, xs: 4..5 }),
            ]
        );
    }
}
