#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate glium;
extern crate lyon;
extern crate image;
extern crate rand;
pub extern crate palette;

pub mod geom;
pub mod sketch;
pub mod shaders;
pub mod element;
mod raster;
mod pipeline;
mod errors;

#[cfg(test)]
mod tests {
    use super::*;
    use geom::shapes::square;
    use pipeline::*;
    use raster::{Tessellate, Tessellation};

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
