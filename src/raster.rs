mod grid_lines;
mod path;
mod regions;
mod surface;

#[cfg(test)]
mod test {
    use super::grid_lines::*;
    use super::path::*;
    use super::regions::*;
    use super::surface::*;
    use crate::geo::*;
    use image::Pixel;
    use std::convert::TryFrom;
    use std::iter::*;

    #[test]
    fn image_output() {
        let mut surface = Surface::with_dimensions(5, 5);
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 3.0),
            V2::new(3.0, 0.0),
        ])
        .expect("triangle");

        let region_list = RegionList::from_iter(vec![triangle]);

        println!("Region list: {:?}", region_list);

        for region in region_list.regions() {
            println!("Region: {:?}", region);
            for shade_command in region {
                println!("\tCommand: {:?}", shade_command);
                surface
                    .pixel(shade_command.x, shade_command.y)
                    .map(|p| p.iter_mut().for_each(|v| *v = 1.0));
            }
        }

        let buffer: FinalBuffer = surface.into();
        buffer.save("test.bmp").expect("To save surface");
    }
}
