mod grid_lines;
mod path;
pub mod regions;
mod sampling;
pub mod surface;

#[cfg(test)]
mod test {
    use super::geo::*;
    use super::grid_lines::*;
    use super::path::*;
    use super::regions::*;
    use super::surface::*;
    use image::Pixel;
    use std::convert::TryFrom;
    use std::iter::*;

    #[test]
    fn image_output() {
        let mut surface = Surface::with_dimensions(5000, 5000);
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 700.0),
            V2::new(3000.0, 0.0),
        ])
        .expect("triangle");

        let region_list = RegionList::from_iter(vec![triangle]);

        for shade_command in region_list.shade_commands() {
            surface.pixel(shade_command.x, shade_command.y).map(|p| {
                p[0] = 1.0;
                p[1] = 1.0;
                p[2] = 1.0;
                p[3] = shade_command.coverage;
            });
        }

        let buffer: FinalBuffer = surface.into();
        buffer.save("test.bmp").expect("To save surface");
    }
}
