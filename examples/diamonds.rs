use itertools::iproduct;
use valora::prelude::*;

fn main() -> Result<()> {
    run_fn(Options::from_args(), |_gpu, world, rng| {
        let fbm = Fbm::new().set_seed(rng.gen());

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            canvas.set_color(LinSrgb::new(1., 1., 1.));
            canvas.paint(Filled(ctx.world));

            let mut rng = ctx.rng.clone();

            let sq_size = 400.;
            let bottom_left = world.center() - V2::new(sq_size / 2., sq_size / 2.);

            let grid_size = 10;
            let sq_size = sq_size / (grid_size as f32);
            let diamond_radius = sq_size / 3.;
            iproduct!(0..grid_size, 0..grid_size)
                .map(|(i, j)| {
                    let x = sq_size * i as f32;
                    let y = sq_size * j as f32;
                    let shift_to_center = sq_size / 2.;
                    let shift_to_center = V2::new(shift_to_center, shift_to_center);

                    let center = bottom_left + P2::new(x, y).to_vector() + shift_to_center;
                    let animated_scale = fbm
                        .noise(P3::new(center.x, center.y, ctx.time.as_secs_f32() / 5.))
                        .abs();

                    (
                        Ngon::diamond(center, diamond_radius * (0.2 + animated_scale))
                            .rotate(center, Angle::radians(ctx.time.as_secs_f32())),
                        x + y,
                    )
                })
                .for_each(|(d, hue)| {
                    canvas.set_color(Hsv::new(hue, 0.7, 0.8));
                    if rng.gen() {
                        canvas.paint(Filled(d));
                    } else {
                        canvas.paint(Stroked {
                            element: d,
                            width: 3.,
                        });
                    }
                });
        })
    })
}
