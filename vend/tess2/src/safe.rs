//! Safe rust wrapper around the subset of libtess2 I personally need.

use super::*;
use itertools::Itertools;
use std::mem;

pub struct Tessellator {
    tess: *mut TESStesselator,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vertex {
    x: f32,
    y: f32,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Triangles {
    vertices: Vec<Vertex>,
    indices: Vec<u32>,
}

pub fn fill(poly: &[Vertex]) -> Result<Triangles, String> {
    if poly.len() < 3 {
        return Err(String::from("A polygon must have at least 3 vertices."));
    }

    let formatted_vertices: Vec<f32> = poly.iter()
        .flat_map(|v| vec![v.x, v.y].into_iter())
        .collect();
    Ok(unsafe {
           use std::os::raw::c_void;
           use std::slice;

           let tess = tessNewTess(0 as *mut TESSalloc);
           tessAddContour(tess,
                          2,
                          (&formatted_vertices[0] as *const f32) as *const c_void,
                          mem::size_of_val(&formatted_vertices[0]) as i32 * 2,
                          poly.len() as i32);
           if tessTesselate(tess,
                            TessWindingRule::TESS_WINDING_NONZERO as i32,
                            TessElementType::TESS_POLYGONS as i32,
                            3,
                            2,
                            0 as *mut TESSreal) != 1 {
               return Err(String::from("Triangulation failed."));
           }

           let raw_triangle_count = tessGetElementCount(tess);
           if raw_triangle_count < 1 {
               return Err(String::from("Triangulation failed to yield triangles."));
           };
           let triangle_count = raw_triangle_count as usize;

           let vertex_buffer = slice::from_raw_parts(tessGetVertices(tess),
                                                     tessGetVertexCount(tess) as usize * 2);
           let triangle_buffer = slice::from_raw_parts(tessGetElements(tess), triangle_count * 3);

           let xs = vertex_buffer.iter().step(2);
           let ys = vertex_buffer.iter().skip(1).step(2);
           let verts = xs.zip(ys);

           let result = Triangles {
               vertices: verts.map(|(x, y)| Vertex { x: *x, y: *y }).collect(),
               indices: triangle_buffer.iter().map(|i| *i as u32).collect(),
           };

           unsafe {
               tessDeleteTess(tess);
           };

           result

       })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(fill(&[Vertex { x: 0.0, y: 0.0 },
                          Vertex { x: 1.0, y: 0.0 },
                          Vertex { x: 1.0, y: 1.0 },
                          Vertex { x: 0.0, y: 1.0 }])
                           .expect("triangulation"),
                   Triangles {
                       vertices: vec![Vertex { x: 0.0, y: 1.0 },
                                      Vertex { x: 1.0, y: 0.0 },
                                      Vertex { x: 1.0, y: 1.0 },
                                      Vertex { x: 0.0, y: 0.0 }],
                       indices: vec![0, 1, 2, 1, 0, 3],
                   });
    }
}