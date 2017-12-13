use geom::Point;
use geom::ellipse::Ellipse;
use geom::poly::Poly;

pub fn tiles(frame: u32, n: usize) -> Vec<Poly> {
    let size = (frame as f32 / n as f32) / frame as f32;
    let intervals = (0..n).map(|i| size * i as f32).collect::<Vec<f32>>();
    intervals
        .clone()
        .into_iter()
        .flat_map(|y| {
                      intervals
                          .clone()
                          .into_iter()
                          .map(|x| Poly::square(Point { x, y }, size))
                          .collect::<Vec<Poly>>()
                  })
        .collect()
}

pub fn dots(frame: u32, n: usize) -> Vec<Ellipse> {
    let size = (frame as f32 / n as f32) / frame as f32;
    let intervals = (0..n + 2)
        .map(|i| size * (i as f32 - 1.0))
        .collect::<Vec<f32>>();
    intervals
        .clone()
        .into_iter()
        .flat_map(|y| {
                      intervals
                          .clone()
                          .into_iter()
                          .map(|x| Ellipse::circle(Point { x, y }, size * 1.2, 0.0))
                          .collect::<Vec<Ellipse>>()
                  })
        .collect()
}