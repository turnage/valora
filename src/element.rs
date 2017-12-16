use raster::Tessellate;
use shaders::Shader;

pub struct Element<'a> {
    pub shader: &'a Shader,
    pub geometry: &'a Tessellate,
}

impl<'a, G: Tessellate> From<&'a (Shader, G)> for Element<'a> {
    fn from(&(ref shader, ref geometry): &'a (Shader, G)) -> Self { Self { shader, geometry } }
}