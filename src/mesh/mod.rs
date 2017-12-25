use color::Colorer;
use errors::Result;
use gpu::{Factory, Gpu, GpuMesh, GpuMeshSpec};
use std::rc::Rc;
use tessellation::Tessellate;

pub struct Mesh<T: Tessellate> {
    pub src: MeshSpec<T>,
    pub mesh: GpuMesh,
    gpu: Rc<Gpu>,
}

#[derive(Clone)]
pub struct MeshSpec<T: Tessellate> {
    pub src: T,
    pub colorer: Colorer,
}

impl<T: Tessellate + Clone> Factory for Mesh<T> {
    type Spec = MeshSpec<T>;
    fn produce(spec: &MeshSpec<T>, gpu: Rc<Gpu>) -> Result<Self> {
        Ok(Self {
               src: spec.clone(),
               gpu: gpu.clone(),
               mesh: GpuMesh::produce(&GpuMeshSpec {
                                           tessellation: spec.src.tessellate()?,
                                           colorer: spec.colorer.clone(),
                                       },
                                      gpu)?,
           })
    }
}