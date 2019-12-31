use valora::prelude::*;
use itertools::iproduct;

fn main() -> Result<()> {
    run_fn(Options::from_args(), |_gpu, world, _rng| {
        Ok(move |ctx: Context, canvas: &mut Canvas| {
            canvas.set_color(LinSrgb::new(1., 1., 1.));
            canvas.paint(Filled(ctx.world));

            let sq_size = 400.;
            let bottom_left = world.center() - V2::new(sq_size / 2., sq_size / 2.);

            let grid_size = 10;
            let sq_size = sq_size / (grid_size as f32);
            iproduct!(0..grid_size, 0..grid_size).map(|(i, j)| {
                let x = sq_size * i as f32;
                let y = sq_size * j as f32;
                let shift_to_center = sq_size / 2.;
                let shift_to_center = V2::new(shift_to_center, shift_to_center);

                let center = bottom_left + P2::new(x, y).to_vector() + shift_to_center;
                (Ngon::diamond(center, sq_size / 3.), x + y)
            }).for_each(|(d, hue)| {
                canvas.set_color(Hsv::new(hue, 0.7, 0.8));
                canvas.paint(Filled(d));
            });
        })
    })
}
