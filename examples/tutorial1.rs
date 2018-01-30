extern crate valora;

use valora::*;

fn main() {
    compose(
        CompositionCfg::from_args(),
        |_, _| -> Result<Composition> {
            let white = Colora::rgb(1.0, 1.0, 1.0, 1.0);
            let red = Colora::rgb(1.0, 0.0, 0.0, 1.0);

            let background = Mesh::from(Rect::frame()).with_color(white);
            let circle = Mesh::from(Ellipse::circle(Point::center(), 0.3)).with_color(red);

            Ok(Composition::new().add(background).add(circle))
        }
    ).expect("working composition")
}