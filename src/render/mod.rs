use element::Element;
use errors::Result;
use glium::texture::RawImage2d;
use raster::Tessellation;
use textures::Texture;

pub enum Renderable {
    Tessellations(Vec<Tessellation>),
    Texture(RawImage2d<'static, u8>),
}

pub struct Render {
    composition: Vec<Renderable>,
}

impl Render {
    pub fn new() -> Self { Render { composition: Vec::new() } }

    pub fn add<'a, T: Into<Element<'a>>>(mut self, r: T) -> Result<Self> {
        self.composition.push(r.into().prerender()?.into());
        Ok(self)
    }

    pub fn add_texture(mut self, t: &Texture) -> Self {
        self.composition.push(t.into());
        self
    }

    pub fn build(self) -> Vec<Renderable> { self.composition }
}

impl Into<Renderable> for Vec<Tessellation> {
    fn into(self) -> Renderable { Renderable::Tessellations(self) }
}

impl<'a> From<&'a Texture> for Renderable {
    fn from(texture: &Texture) -> Renderable {
        let &Texture(ref img) = texture;
        let dims = img.dimensions();

        Renderable::Texture(RawImage2d::from_raw_rgb(img.clone().into_raw(), dims))
    }
}