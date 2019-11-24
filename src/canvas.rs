//! A painting surface.

use crate::{gpu::Shader, paint::Paint, Element, Method, P2};
use lyon_path::Builder;
use palette::{Alpha, IntoColor, LinSrgb, LinSrgba};

/// A painting surface.
pub struct Canvas {
    path: Builder,
    shader: Shader,
    color: LinSrgba,
    stroke_thickness: f32,
    scale: f32,
    elements: Vec<Element>,
}

impl Canvas {
    pub(crate) fn new(default_shader: Shader, scale: f32) -> Self {
        Self {
            path: Builder::new(),
            shader: default_shader,
            color: Alpha::<LinSrgb, _>::new(1., 1., 1., 1.),
            scale,
            stroke_thickness: 1.,
            elements: vec![],
        }
    }

    /// Paints an element.
    pub fn paint(&mut self, element: impl Paint) { element.paint(self); }

    /// Sets the current color.
    pub fn set_color(&mut self, color: impl IntoColor) {
        self.color = Alpha::from(color.into_rgb());
    }

    /// Sets the current color.
    pub fn set_color_with_alpha(&mut self, color_alpha: Alpha<impl IntoColor, f32>) {
        self.color = Alpha {
            color: color_alpha.color.into_rgb(),
            alpha: color_alpha.alpha,
        };
    }

    /// Stats a new path at the given point.
    pub fn move_to(&mut self, dest: P2) {
        self.path = Builder::new();
        self.path.move_to(dest * self.scale);
    }

    /// Adds a line to the current path which ends at the given point.
    pub fn line_to(&mut self, dest: P2) { self.path.line_to(dest * self.scale); }

    /// Adds a quadratic bezier curve to the current path with the given control and end points.
    pub fn quadratic_to(&mut self, ctrl: P2, end: P2) {
        self.path
            .quadratic_bezier_to(ctrl * self.scale, end * self.scale);
    }

    /// Adds a cubic bezier curve to the current path with the given control and end points.
    pub fn cubic_to(&mut self, ctrl0: P2, ctrl1: P2, end: P2) {
        self.path
            .cubic_bezier_to(ctrl0 * self.scale, ctrl1 * self.scale, end * self.scale);
    }

    /// Closes the current path.
    pub fn close(&mut self) { self.path.close() }

    /// Sets the thickness of lines drawn with the `stroke()`.
    pub fn set_stroke_thickness(&mut self, stroke_thickness: f32) {
        self.stroke_thickness = stroke_thickness;
    }

    /// Paints the current path by filling the region inside the path.
    pub fn fill(&mut self) { self.push_element(Method::Fill); }

    /// Paints the current path by stroking the path.
    pub fn stroke(&mut self) { self.push_element(Method::Stroke(self.stroke_thickness)); }

    /// Sets the current shader used to shade rastered paths.
    ///
    /// Changing shaders requires making a new draw call to the GPU and tearing down some state.
    /// Changing shaders 0-10 times per frame is likely to be fast enough. Changing shaders 500
    /// times per frame will be slow.
    pub fn set_shader(&mut self, shader: Shader) { self.shader = shader; }

    fn push_element(&mut self, raster_method: Method) {
        let mut path = Builder::new();
        std::mem::swap(&mut self.path, &mut path);

        self.elements.push(Element {
            path,
            color: self.color,
            shader: self.shader.clone(),
            raster_method,
        });
    }
}

impl IntoIterator for Canvas {
    type Item = Element;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter { self.elements.into_iter() }
}
