use mesh::Mesh;
use std::collections::HashMap;

pub fn one_to_n<F>(src: &Mesh, n: Vec<Mesh>, f: F) -> Vec<Mesh>
where
    F: Fn(&Mesh, Mesh) -> Mesh,
{
    n.into_iter().map(|n| f(src, n)).collect()
}
