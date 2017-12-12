use element::Element;
use errors::Result;
use raster::Tessellation;

pub enum Renderable {
    Tessellations(Vec<Tessellation>),
    Texture,
}

pub struct Render {
    composition: Vec<Renderable>,
}

impl Render {
    pub fn new() -> Self { Render { composition: Vec::new() } }

    pub fn add<T: Into<Element>>(mut self, r: T) -> Result<Self> {
        self.composition.push(r.into().prerender()?.into());
        Ok(self)
    }

    pub fn build(self) -> Vec<Renderable> { self.composition }
}

impl Into<Renderable> for Vec<Tessellation> {
    fn into(self) -> Renderable { Renderable::Tessellations(self) }
}