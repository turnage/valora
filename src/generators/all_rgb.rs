use palette::Rgb;

pub fn all_rgb() -> Vec<Rgb> {
    use num::PrimInt;

    let mut all = Vec::with_capacity(256.pow(3));
    let f = |c: usize| c as f32 / 255.0;
    for r in 0..255 {
        for g in 0..255 {
            for b in 0..255 {
                all.push(Rgb::new(f(r), f(g), f(b)));
            }
        }
    }
    all
}