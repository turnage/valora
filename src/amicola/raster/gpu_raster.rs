use crate::amicola::geo::{Polygon, V4};
use crate::amicola::raster::regions::{Region, RegionList, ShadeCommand};
use crate::amicola::RasterTarget;
use crate::amicola::{Element, RasterMethod, Shader};
use itertools::{Either, Itertools};
use luminance::{
    context::GraphicsContext,
    framebuffer::Framebuffer,
    render_state::RenderState,
    shader::program::{Program, Uniform},
    tess::{Mode, Tess, TessBuilder, TessSliceIndex},
    texture::{Dim2, Flat},
};
use luminance_derive::{Semantics as VertexSemantics, UniformInterface, Vertex};
use luminance_glfw::{GlfwSurface, Surface as LuminanceSurface, WindowDim, WindowEvent, WindowOpt};
use std::{convert::TryFrom, rc::Rc};

#[derive(Clone, Copy, Debug, Eq, PartialEq, VertexSemantics)]
pub enum Semantics {
    #[sem(name = "position", repr = "[f32; 2]", wrapper = "VertexPosition")]
    Position,
    #[sem(name = "color", repr = "[f32; 4]", wrapper = "VertexColor")]
    Color,
}

#[derive(Debug, UniformInterface)]
struct UniformSet {
    width: Uniform<f32>,
    height: Uniform<f32>,
}

const VERTEX_SHADER: &str = include_str!("../../shaders/default.vert");
const FRAGMENT_SHADER: &str = include_str!("../../shaders/default.frag");

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Vertex)]
#[vertex(sem = "Semantics")]
struct Vertex {
    position: VertexPosition,
    #[vertex(normalized = "true")]
    color: VertexColor,
}

pub struct GpuTarget {
    surface: GlfwSurface,
    program: Rc<Program<Semantics, (), UniformSet>>,
    framebuffer: Framebuffer<Flat, Dim2, (), ()>,
    width: f32,
    height: f32,
}

struct Tessellation {
    boundaries: Tess,
    spans: Tess,
}

impl GpuTarget {
    pub fn with_dimensions(width: u32, height: u32) -> Self {
        let mut surface = GlfwSurface::new(
            WindowDim::Windowed(width, height),
            "valora",
            WindowOpt::default(),
        )
        .expect("Glfw surface");

        let program = Rc::new(
            Program::<Semantics, (), UniformSet>::from_strings(
                None,
                VERTEX_SHADER,
                None,
                FRAGMENT_SHADER,
            )
            .expect("Compile default shader")
            .ignore_warnings(),
        );

        let framebuffer = surface.back_buffer().unwrap();

        GpuTarget {
            surface,
            program,
            framebuffer,
            width: width as f32,
            height: height as f32,
        }
    }

    pub fn events<'a>(&'a mut self) -> Box<dyn Iterator<Item = WindowEvent> + 'a> {
        self.surface.poll_events()
    }

    pub fn show(&mut self) {
        self.surface.swap_buffers();
    }

    fn tessellate<'a>(
        &'a mut self,
        rgba: V4,
        shade_commands: impl Iterator<Item = ShadeCommand> + 'a,
    ) -> Tessellation {
        let (boundaries, spans): (Vec<Vertex>, Vec<[Vertex; 2]>) =
            shade_commands.partition_map(move |command| match command.region {
                Region::Boundary { x, y } => Either::Left(Vertex {
                    position: VertexPosition::new([x as f32, y as f32]),
                    color: VertexColor::new([
                        rgba.x as f32,
                        rgba.y as f32,
                        rgba.z as f32,
                        (rgba.w * command.coverage) as f32,
                    ]),
                }),
                Region::Fill {
                    start_x, end_x, y, ..
                } => Either::Right([
                    Vertex {
                        position: VertexPosition::new([start_x as f32, y as f32]),
                        color: VertexColor::new([
                            rgba.x as f32,
                            rgba.y as f32,
                            rgba.z as f32,
                            rgba.w as f32,
                        ]),
                    },
                    Vertex {
                        position: VertexPosition::new([(end_x - 1) as f32, y as f32]),
                        color: VertexColor::new([
                            rgba.x as f32,
                            rgba.y as f32,
                            rgba.z as f32,
                            rgba.w as f32,
                        ]),
                    },
                ]),
            });

        let boundaries = TessBuilder::new(&mut self.surface)
            .add_vertices(boundaries)
            .set_mode(Mode::Point)
            .build()
            .unwrap();

        let spans = TessBuilder::new(&mut self.surface)
            .add_vertices(
                spans
                    .into_iter()
                    .flat_map(|v| v.to_vec())
                    .collect::<Vec<Vertex>>(),
            )
            .set_mode(Mode::Line)
            .build()
            .unwrap();

        Tessellation { boundaries, spans }
    }
}

impl RasterTarget for GpuTarget {
    fn raster(&mut self, mut element: Element) {
        match element.raster_method {
            RasterMethod::Fill => {
                let poly = match Polygon::try_from(element.path) {
                    Ok(poly) => poly,
                    // An unclosed path has no fill.
                    _ => return,
                };

                let rgba = match element.shader {
                    Shader::Solid(rgba) => rgba,
                };
                let program = self.program.clone();

                let w = self.width;
                let h = self.height;

                let tessellation = self.tessellate(rgba, RegionList::from(poly).shade_commands());

                self.surface.pipeline_builder().pipeline(
                    &self.framebuffer,
                    [1., 1., 1., 1.],
                    |_, mut shade_gate| {
                        shade_gate.shade(program.as_ref(), |uniforms, mut render_gate| {
                            uniforms.width.update(w);
                            uniforms.height.update(h);
                            render_gate.render(RenderState::default(), move |mut tess_gate| {
                                tess_gate.render(tessellation.boundaries.slice(..));
                                tess_gate.render(tessellation.spans.slice(..));
                            });
                        });
                    },
                );
            }
            _ => unimplemented!(),
        };
    }
}
