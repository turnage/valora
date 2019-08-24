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

    #[test]
    fn image_output() {
        let mut surface = Surface::with_dimensions(200, 200);
        let triangle = Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(0.0, 100.0),
            V2::new(100.0, 0.0),
        ])
        .expect("triangle");

        let mut region_list = RegionList::new();
        region_list.push(triangle);

        println!("Region list: {:?}", region_list);

        for region in region_list.regions() {
            match region {
                Region::Boundary { x, y } => {
                    println!("Shading boundary region {:?}v{:?}", x, y);
                    surface
                        .pixel(x, 199 - y)
                        .map(|p| p.iter_mut().for_each(|v| *v = 1.0));
                }
                e => {
                    println!("Other boundary: {:?}", e);
                }
            }
        }

        let buffer: FinalBuffer = surface.into();
        buffer.save("test.bmp").expect("To save surface");
    }
}
