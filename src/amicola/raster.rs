mod grid_lines;
mod path;
mod regions;
mod sampling;
pub mod surface;

use self::regions::RegionList;
use self::surface::Surface;
use crate::amicola::geo::{Polygon, V2};
use crate::amicola::{Element, RasterMethod, Shader};
use std::convert::TryFrom;
use std::iter::FromIterator;

use palette::{rgb::LinSrgba, Blend};

/// Rasters an element.
pub fn raster(surface: &mut Surface, mut element: Element) {
    match element.raster_method {
        RasterMethod::Fill => {
            let poly = match Polygon::try_from(element.path) {
                Ok(poly) => poly,
                // An unclosed path has no fill.
                _ => return,
            };

            for shade_command in RegionList::from(poly).shade_commands() {
                match &element.shader {
                    Shader::Solid(color) => {
                        surface.pixel(shade_command.x, shade_command.y).map(|p| {
                            let dest = LinSrgba::new(p[0], p[1], p[2], p[3]);
                            let src = LinSrgba::new(
                                color.x,
                                color.y,
                                color.z,
                                shade_command.coverage * color.w,
                            );
                            let (r, g, b, a) = src.over(dest).into_components();

                            p[0] = r;
                            p[1] = g;
                            p[2] = b;
                            p[3] = a;
                        })
                    }
                    Shader::Dynamic(f) => {
                        surface.pixel(shade_command.x, shade_command.y).map(|p| {
                            let color = f(V2::new(shade_command.x as f64, shade_command.y as f64));
                            p[0] = color.x;
                            p[1] = color.y;
                            p[2] = color.z;
                            p[3] = shade_command.coverage * color.w;
                        })
                    }
                };
            }
        }
        _ => unimplemented!(),
    };
}
