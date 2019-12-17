#![feature(clamp)]

use valora::prelude::*;

#[derive(UniformSet, Copy, Clone, Debug)]
struct Uniforms {
    time: f32,
}

fn main() -> Result<()> {
    run_fn(Options::from_args(), |gpu, world, rng| {
        let mut program = ShaderProgram::new(&gpu, "examples/noise.frag")?;
        let mut uniforms = Uniforms { time: 0. };

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            canvas.set_color(LinSrgb::new(1., 1., 1.));
            canvas.paint(Filled(ctx.world));

            uniforms.time = ctx.time.as_secs_f32().sin() * 10.;
            canvas.set_shader(program.bind(uniforms));

            let square = Ngon::square(world.center(), 200.);
            let outer_square = Ngon::square(world.center(), 250.);

            canvas.paint(Filled(square));
            canvas.paint(Stroked {
                width: 40.,
                element: outer_square,
            });
        })
    })
}
