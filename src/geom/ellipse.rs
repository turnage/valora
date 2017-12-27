use errors::Result;
use geom::{Centered, Place, Point, Scale, Translate};
use lyon::math::Radians;
use tessellation::{Tessellate, Tessellation};

#[derive(Debug, Clone)]
pub struct Ellipse {
    pub center: Point,
    pub width: f32,
    pub height: Option<f32>,
    pub rotation: Radians<f32>,
    pub tolerance: Option<f32>,
}

impl Ellipse {
    pub fn circle(center: Point, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            rotation: Radians::new(0.0),
            tolerance: None,
        }
    }
}

impl Scale for Ellipse {
    fn scale(self, scale: f32) -> Self {
        Self { width: self.width * scale, height: self.height.map(|h| h * scale), ..self }
    }
}

impl Centered for Ellipse {
    fn centroid(&self) -> Point { self.center }
}

impl Place for Ellipse {
    fn place(self, dest: Point) -> Self { Self { center: dest, ..self } }
}

impl Translate for Ellipse {
    fn translate(self, delta: Point) -> Self { Self { center: self.center + delta, ..self } }
}

impl Tessellate for Ellipse {
    fn tessellate(&self) -> Result<Tessellation> {
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{VertexBuffers, simple_builder};
        use lyon::path_iterator::math::Vec2;

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        match self.height {
            Some(height) => {
                basic_shapes::fill_ellipse(self.center.into(),
                                           Vec2::new(self.width, height),
                                           self.rotation,
                                           self.tolerance.unwrap_or(0.000001),
                                           &mut simple_builder(&mut vertex_buffers));
            }
            None => {
                basic_shapes::fill_circle(self.center.into(),
                                          self.width,
                                          self.tolerance.unwrap_or(0.000001),
                                          &mut simple_builder(&mut vertex_buffers));
            }
        }
        Ok(Tessellation {
               vertices: vertex_buffers
                   .vertices
                   .into_iter()
                   .map(|p| p.position)
                   .collect(),
               indices: vertex_buffers
                   .indices
                   .into_iter()
                   .map(Into::into)
                   .collect(),
           })
    }
}