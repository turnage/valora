#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate gfx;
extern crate gfx_device_gl;
extern crate gfx_window_glutin;
extern crate glutin;
extern crate lyon;

pub mod geom;
pub mod sketch;
mod raster;
mod pipeline;
mod errors;

#[cfg(test)]
mod tests {
    use super::*;
    use pipeline::*;
    use raster::{Tessellate, Tessellation};
    use geom::shapes::square;

    fn test_tessellations() -> Vec<Tessellation> {
        /*let sq = geom::poly::Poly {
            vertices: vec![
                geom::Point { x: 0.0, y: 0.0 },
                geom::Point { x: 0.5, y: 0.0 },
                geom::Point { x: 0.5, y: 0.5 },
                geom::Point { x: 0.0, y: 0.5 },
            ],
        };*/
        let sq = square(geom::Point { x: 0.0, y: 0.0 }, 0.5);
        vec![sq.tessellate().expect("square tesselation")]
    }

    #[test]
    fn it_works() {
        let pipeline = Pipeline::new(500).expect("valid pipeline");

        let mut cycle = pipeline.events();
        while let Ok(Some((mut pipeline, events))) = cycle {
            pipeline.draw(test_tessellations());
            cycle = pipeline.events();
        }
    }
}
