use gpu::Shader;
use mesh::{Mesh, Instancer};
use poly::Rect;
use palette::Colora;
use std::mem::swap;

#[derive(Debug)]
pub enum Layer {
    Mesh { shader: Shader, mesh: Mesh },
    MeshGroup { shader: Shader, meshes: Vec<Mesh> },
    MeshInstances { shader: Shader, src: Mesh, meshes: Vec<Mesh> },
}

impl From<Mesh> for Layer {
    fn from(mesh: Mesh) -> Self {
        Layer::Mesh {shader: Shader::Default, mesh }
    }
}

impl From<Instancer> for Layer {
    fn from(instancer: Instancer) -> Self {
        Layer::MeshInstances {shader: Shader::Default, src: instancer.src, meshes: instancer.instances }
    }
}

impl<S: Into<Shader>> From<S> for Layer {
    fn from(shader_src: S) -> Self {
        Layer::Mesh {
            shader: shader_src.into(),
            mesh: Mesh::from(Rect::frame()),
        }
    }
}

impl<S: Into<Shader>> From<(S, Mesh)> for Layer {
    fn from((shader, mesh): (S, Mesh)) -> Self {
        Layer::Mesh {
            shader: shader.into(),
            mesh,
        }
    }
}

pub enum LayerInput {
    Single(Layer),
    Many(Vec<Layer>),
}

impl From<Vec<Mesh>> for LayerInput {
    fn from(meshes: Vec<Mesh>) -> Self {
         let mut layers = Vec::new();
         let mut meshes_in_layer: Vec<Mesh> = Vec::new();
         for mesh in meshes {
            if meshes_in_layer.is_empty() || meshes_in_layer[0].blend_mode == mesh.blend_mode {
                meshes_in_layer.push(mesh);
            } else {
                layers.push(Layer::MeshGroup { shader: Shader::Default, meshes: meshes_in_layer });
                meshes_in_layer = Vec::new();
            }
         }
         if !meshes_in_layer.is_empty() {
            layers.push(Layer::MeshGroup { shader: Shader::Default, meshes: meshes_in_layer });
         }
         LayerInput::Many(layers)
    }
}

impl<T: Into<Layer>> From<T> for LayerInput {
    fn from(t: T) -> Self {
        LayerInput::Single(t.into())
    }
}

impl Iterator for LayerInput {
    type Item = Layer;
    fn next(&mut self) -> Option<Layer> {
        let mut output = None;
        let mut tmp = LayerInput::Many(Vec::new());
        swap(self, &mut tmp);
        *self = match tmp {
            LayerInput::Single(il) => {
                output = Some(il);
                LayerInput::Many(Vec::new())
            }
            LayerInput::Many(mut ts) => {
                output = ts.pop();
                LayerInput::Many(ts)
            }
        };
        output
    }
}

#[derive(Default)]
pub struct Composition {
    layers: Vec<Layer>,
}

impl Composition {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn solid_layer(self, color: Colora) -> Self {
        self.add(Mesh::from(Rect::frame()).with_color(color))
    }

    pub fn add<L: Into<LayerInput>>(mut self, layer: L) -> Self {
        self.layers.extend(layer.into());
        self
    }

    pub fn layers(self) -> Vec<Layer> {
        self.layers
    }
}
