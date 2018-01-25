use poly::{Poly, Point};

/// The points in the grid tiles to return.
#[derive(Clone, Copy, Debug)]
pub enum GridPoints {
    /// The grid will be defined using the bottom lefts of the grid tiles.
    BottomLefts,
    /// The grid will be defined using the center of the grid tiles.
    Centers
}
#[derive(Clone, Copy, Debug)]
pub struct GridCfg {
    pub points: GridPoints,
    pub width: f32,
    pub height: f32,
    pub tiles_wide: usize,
    pub tiles_high: usize,
    pub center: Point,
}

pub fn frame_grid(n: usize, points: GridPoints) -> (f32, Vec<Point>) {
    (1.0 / n as f32, grid(&GridCfg{
        points,
        width: 1.0,
        height: 1.0,
        tiles_wide: n,
        tiles_high: n,
        center: Point::center(),
    }))
}

pub fn grid(cfg: &GridCfg) -> Vec<Point> {
    let tile_width = cfg.width / cfg.tiles_wide as f32;
    let tile_height = cfg.height / cfg.tiles_high as f32;
    let points = (0..(cfg.tiles_wide * cfg.tiles_high)).into_iter().map(|i| {
        let x = i % cfg.tiles_wide;
        let y = i / cfg.tiles_wide;
        let (tx, ty) = (x as f32 * tile_width, y as f32 * tile_height);
        match cfg.points {
            GridPoints::BottomLefts => Point { x: tx, y: ty},
            GridPoints::Centers => Point { x: tx + tile_width / 2.0, y: ty + tile_height / 2.0 },
        }
    }).collect();
    Poly::Irregular(points).place(cfg.center).vertices()
}