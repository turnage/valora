//! Fitting frames into textures.

/// Shrinks RGBA8 pixels to at most `max_side` pixels on each side by
/// averaging square blocks of pixels. Returns the new width, height, and
/// pixels, or `None` if the pixels already fit.
pub fn shrink_to_fit(
    width: usize,
    height: usize,
    rgba: &[u8],
    max_side: usize,
) -> Option<(usize, usize, Vec<u8>)> {
    let factor = width.max(height).div_ceil(max_side.max(1));
    if factor <= 1 {
        return None;
    }

    let (new_width, new_height) = (width.div_ceil(factor), height.div_ceil(factor));
    let mut shrunk = Vec::with_capacity(new_width * new_height * 4);
    for block_y in 0..new_height {
        for block_x in 0..new_width {
            let mut sum = [0u32; 4];
            let mut count = 0;
            for y in block_y * factor..((block_y + 1) * factor).min(height) {
                for x in block_x * factor..((block_x + 1) * factor).min(width) {
                    let pixel = &rgba[(y * width + x) * 4..][..4];
                    sum.iter_mut().zip(pixel).for_each(|(s, c)| *s += *c as u32);
                    count += 1;
                }
            }
            shrunk.extend(sum.map(|s| (s / count) as u8));
        }
    }
    Some((new_width, new_height, shrunk))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fitting_frames_are_untouched() {
        assert_eq!(shrink_to_fit(4, 2, &[0; 32], 4), None);
    }

    #[test]
    fn averages_blocks() {
        #[rustfmt::skip]
        let rgba = [
            0, 0, 0, 255,   100, 0, 0, 255,   7, 7, 7, 7,
            0, 0, 0, 255,   100, 0, 0, 255,   9, 9, 9, 9,
        ];
        assert_eq!(
            shrink_to_fit(3, 2, &rgba, 2),
            Some((2, 1, vec![50, 0, 0, 255, 8, 8, 8, 8]))
        );
    }
}
