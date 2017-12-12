use errors::Result;
use geom::Geometry;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

pub enum Element {
    Geometry((Shader, Vec<Geometry>)),
}

impl Element {
    pub fn prerender(self) -> Result<Vec<Tessellation>> {
        match self {
            Element::Geometry((shader, geometries)) => {
                geometries
                    .into_iter()
                    .map(|g| g.tessellate(&shader))
                    .collect()
            }
        }
    }
}

impl Into<Element> for (Shader, Vec<Geometry>) {
    fn into(self) -> Element { Element::Geometry(self) }
}