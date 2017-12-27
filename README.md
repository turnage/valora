# valora

A declarative generative art toolkit!

[Here's a demo](https://i.imgur.com/aLAJrKH.gif). (Code below)

See some actual pieces I've made with it [here](https://www.instagram.com/venlute/).

````rust
extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::StdRng;

/// To render a sketch in Valora we must implement the Sketch trait on the type.
/// The trait function takes a self reference so the type can serve as a config
/// for the sketch.
struct CircleSketch {
    radius: f32,
    count: usize,
}

impl Sketch for CircleSketch {
    /// The sketch method is called ONCE, ever. Valora is declarative, so we
    /// do not define how to make the composition, just what it is, even in the
    /// case of animations.
    ///
    /// The valora coordinate space spans from (0,0) in the bottom left to
    /// (1, 1) in the top right. This is unorthodox but hopefully you will see
    /// that it makes lots of math very convenient.
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        /// This sketch will use two meshes. A mesh is a set of vertices that
        /// we can render, similar to a shape in Processing except the colors of
        /// the vertices are an attribute of the mesh, rather than a side
        /// effect of mutable state.
        let transparent_red_dot = Mesh {
            /// This mesh will be generated from an Ellipse, and this Mesh
            /// inherits all the traits Ellipses implement.
            src: Ellipse::circle(Point::center(), self.radius),
            /// A colorer determines how to color the mesh vertices when it is
            /// generated for rendering. In these cases all vertices will be
            /// colored the same, using the Colora we pass the colorer. Colora
            /// is an alpha'd color type from Ogeon's palette library. It can
            /// be defined in almost any color space; we just happen to be
            /// using rgb here (last value is alpha).
            colorer: Colorer::from(Colora::rgb(1.0, 0.0, 0.0, 0.7)),
        };
        let blue_dot =
            Mesh { src: Ellipse::circle(Point::center(), self.radius), colorer: Colorer::blue() };

        /// We can instance any geometry supporting the "Place" trait, which
        /// allows us to place it at a point. This is both a convenient
        /// abstraction and a graphics programming optimization that means
        /// we don't have to upload many copies of geometry to the graphics
        /// card if we are only changing certain properties.
        let red_instancer = Instancer::new(transparent_red_dot);
        let blue_instancer = Instancer::new(blue_dot);

        /// Here we collect a set of random points from Valora's patterns
        /// module. Rect::frame is a rectangle spanning the entire sketch frame.
        let red_spawn_points = sparkles(self.count, &Rect::frame(), &mut rng);
        let blue_spawn_points = sparkles(self.count, &Rect::frame(), &mut rng);

        /// We use our instancer here to spawn the ellipses we made earlier at
        /// all of our spawn points. We don't need a list of points here; we
        /// could spawn using any type implementing the "SpawnSrc" trait. It
        /// works similar to instancing on meshes in Blender.
        let red_dots = spawn(&red_instancer, &red_spawn_points);
        let blue_dots = spawn(&blue_instancer, &blue_spawn_points);

        /// We declare our ellipses will tween scale from 100% to 150% on a
        /// sine oscillation with a period of 100 frames.
        let dots: Vec<Tween<Mesh<Ellipse>>> = red_dots
            .into_iter()
            .chain(blue_dots.into_iter())
            .enumerate()
            .map(|(i, dot)| {
                Tween::from(dot).anim_scale(1.0,
                                            1.5,
                                            Interpolation::Oscillation {
                                                oscillation: Oscillation::Sine,
                                                start: i,
                                                period: 100,
                                            })
            })
            .collect();

        /// We draw a background by coloring a frame sized rectangle black,
        /// then compose our dots on top.
        Ok(Composition::new()
               .add(Mesh { src: Rect::frame(), colorer: Colorer::black() })
               .add(dots))
    }
}

fn main() {
    /// By providing a root_frame_filename we've told valora we want to save
    /// the sketch's frames to a directory of that name, further subdirectory'd
    /// under the seed of the run with frames named by their frame number as
    /// pngs for convenient piping into `convert` or `ffmpeg`.
    ///
    /// Press 'r' while watching the sketch to reseed!
    sketch(SketchCfg { size: 500, root_frame_filename: Some(String::from("circle")), seed: None },
           CircleSketch { radius: 0.05, count: 25 })
            .expect("sketch");
}
````