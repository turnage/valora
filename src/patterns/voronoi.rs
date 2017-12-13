use geom::Point;
use palette::{Blend, Colora};
use rand::{self, Rng};
use shaders::Shader;
use std;
use textures::Texture;

pub fn voronoi(size: u32, sites: Vec<(Colora, Point)>) -> Texture {
    let shader = move |point: Point| {
        sites
            .iter()
            .min_by(|&&(_, p1), &&(_, p2)| if point.raw_distance(p1) * point.manhattan(p1) >
                                              point.raw_distance(p2) * point.manhattan(p2) {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Less
                    })
            .unwrap()
            .0
    };
    Texture::from_shader(size * 4, &Shader::linear(shader))
}