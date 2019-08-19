mod buffer;
mod grid_lines;
mod path;
mod regions;

#[cfg(test)]
mod test {
    use super::buffer::*;
    use super::grid_lines::*;
    use super::path::*;
    use super::regions::*;
    use crate::geo::*;
    use image::Pixel;
    use std::convert::TryFrom;

    #[test]
    fn image_output() {
        let mut buffer = Buffer::new(200, 200);

        let mut region_list = RegionList::new();
        region_list.push(
            Polygon::try_from(vec![
                V2::new(0.0, 0.0),
                V2::new(0.0, 100.0),
                V2::new(100.0, 0.0),
            ])
            .expect("triangle"),
        );

        println!("Region list: {:?}", region_list);

        for region in region_list.regions() {
            match region {
                Region::Boundary { x, y } => {
                    println!("Shading boundary region {:?}v{:?}", x, y);
                    buffer.get_pixel_mut(x as u32, y as u32).map(|_| 1.0);
                }
                _ => {}
            }
        }

        finalize_buffer(buffer)
            .save("test.bmp")
            .expect("To save buffer");
    }
}
