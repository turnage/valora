use color::Colorer;
use gpu::Shader;
use mesh::{Mesh, Instancer};
use poly::Rect;
use std::mem::swap;

pub enum Layer {
    Mesh { shader: Shader, mesh: Mesh },
    MeshGroup { shader: Shader, src: Mesh, meshes: Vec<Mesh> },
}

impl From<Mesh> for Layer {
    fn from(mesh: Mesh) -> Self {
        Layer::Mesh {shader: Shader::Default, mesh }
    }
}

impl From<Instancer> for Layer {
    fn from(instancer: Instancer) -> Self {
        Layer::MeshGroup {shader: Shader::Default, src: instancer.src, meshes: instancer.instances }
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

impl<T: Into<Layer>> From<T> for LayerInput {
    fn from(t: T) -> Self {
        LayerInput::Single(t.into())
    }
}

impl<T: Into<Layer>> From<Vec<T>> for LayerInput {
    fn from(ts: Vec<T>) -> Self {
        LayerInput::Many(ts.into_iter().map(Into::into).collect())
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

    pub fn solid_layer(self, colorer: Colorer) -> Self {
        self.add(Mesh::from(Rect::frame()).with_colorer(colorer))
    }

    pub fn add<L: Into<Layer>>(mut self, layer: L) -> Self {
        self.layers.push(layer.into());
        self
    }

    pub fn layers(self) -> Vec<Layer> {
        self.layers
    }
}
