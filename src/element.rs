use errors::Result;
use geom::Geometry;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

pub enum Element<'a> {
    Geometry(&'a (Shader, Vec<Geometry>)),
}

impl<'a> Element<'a> {
    pub fn prerender(self) -> Result<Vec<Tessellation>> {
        match self {
            Element::Geometry(&(ref shader, ref geometries)) => {
                geometries
                    .into_iter()
                    .map(|g| g.tessellate(&shader))
                    .collect()
            }
        }
    }
}

impl<'a> From<&'a (Shader, Vec<Geometry>)> for Element<'a> {
    fn from(src: &(Shader, Vec<Geometry>)) -> Element { Element::Geometry(src) }
}