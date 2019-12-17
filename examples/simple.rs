#![feature(clamp)]

use valora::prelude::*;

#[derive(UniformSet, Copy, Clone, Debug)]
struct Uniforms {
    /// A solid color overlay on the shader pattern.
    color: (f32, f32, f32),
    /// The vector scale of the output.
    scale: f32,
    /// Width of the output.
    width: f32,
    /// Height of the output.
    height: f32,
}

fn main() -> Result<()> {
    run_fn(Options::from_args(), |gpu, world, rng| {
        let mut program = ShaderProgram::new(&gpu, "examples/pattern.frag")?;
        let mut uniforms = Uniforms {
            color: Hsv::new(0., 0.7, 0.7).into_rgb::<Srgb>().into_components(),
            scale: world.scale,
            width: world.width,
            height: world.height,
        };

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            canvas.set_color(LinSrgb::new(1., 1., 1.));
            canvas.paint(Filled(ctx.world));

            let hue = ctx.time.as_secs_f32().sin().abs() * 40.;
            uniforms.color = Hsv::new(hue, 0.7, 0.7).into_rgb::<Srgb>().into_components();
            canvas.set_shader(program.bind(uniforms));
            let square = Ngon::square(world.center(), 200.);

            canvas.paint(Filled(square));
        })
    })
}
