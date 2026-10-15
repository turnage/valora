//! A painting surface for paths and polygons.

use crate::{
    edge::path_edges,
    raster::{FillRule, Raster},
    sampling::SampleDepth,
};
use geo_types::{LineString, Polygon};
use kurbo::{Arc, BezPath, Cap, Join, ParamCurve, Point, Stroke, StrokeOpts, Vec2};

/// The maximum distance, in pixels, between a curve and the line segments
/// approximating it.
const TOLERANCE: f64 = 0.025;

/// A painting surface which rasterizes paths into [`Raster`]s.
///
/// Paths are built up with `move_to`, `line_to`, and friends, then consumed by
/// [`Canvas::fill`] or [`Canvas::stroke`]. Coordinates are in pixels, and the
/// raster bounds span from the origin to `(width, height)`.
pub struct Canvas {
    width: u32,
    height: u32,
    path: BezPath,
    current: Option<Point>,
    subpath_start: Option<Point>,
    subpath_open: bool,
    stroke_width: f64,
    fill_rule: FillRule,
    sample_depth: SampleDepth,
}

impl Canvas {
    /// Creates a canvas whose rasters are clipped to `width` x `height`
    /// pixels.
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            width,
            height,
            path: BezPath::new(),
            current: None,
            subpath_start: None,
            subpath_open: false,
            stroke_width: 1.,
            fill_rule: FillRule::default(),
            sample_depth: SampleDepth::default(),
        }
    }

    /// Sets how many samples decide the opacity of pixels at the boundary of
    /// a raster area.
    pub fn set_sample_depth(&mut self, sample_depth: SampleDepth) {
        self.sample_depth = sample_depth;
    }

    /// Sets the rule deciding which regions of a path `fill()` shades.
    pub fn set_fill_rule(&mut self, fill_rule: FillRule) {
        self.fill_rule = fill_rule;
    }

    /// Sets the width of lines drawn with `stroke()`.
    pub fn set_stroke_width(&mut self, stroke_width: f64) {
        self.stroke_width = stroke_width;
    }

    /// Starts a new subpath at the given point.
    pub fn move_to(&mut self, dest: impl Into<Point>) {
        let dest = dest.into();
        self.path.move_to(dest);
        self.current = Some(dest);
        self.subpath_start = Some(dest);
        self.subpath_open = true;
    }

    /// Adds a line to the current path which ends at the given point.
    pub fn line_to(&mut self, dest: impl Into<Point>) {
        let dest = dest.into();
        if self.continue_subpath(dest) {
            self.path.line_to(dest);
            self.current = Some(dest);
        }
    }

    /// Adds a quadratic bezier curve to the current path with the given
    /// control and end points.
    pub fn quadratic_to(&mut self, ctrl: impl Into<Point>, end: impl Into<Point>) {
        let (ctrl, end) = (ctrl.into(), end.into());
        self.continue_subpath(ctrl);
        self.path.quad_to(ctrl, end);
        self.current = Some(end);
    }

    /// Adds a cubic bezier curve to the current path with the given control
    /// and end points.
    pub fn cubic_to(
        &mut self,
        ctrl0: impl Into<Point>,
        ctrl1: impl Into<Point>,
        end: impl Into<Point>,
    ) {
        let (ctrl0, ctrl1, end) = (ctrl0.into(), ctrl1.into(), end.into());
        self.continue_subpath(ctrl0);
        self.path.curve_to(ctrl0, ctrl1, end);
        self.current = Some(end);
    }

    /// Adds an elliptical arc to the path, starting from the current point and
    /// sweeping `sweep` radians around `center`. The ellipse's x axis is
    /// rotated by `x_rotation` radians.
    ///
    /// Without a current point, the arc starts at angle zero.
    pub fn arc(
        &mut self,
        center: impl Into<Point>,
        radii: impl Into<Vec2>,
        sweep: f64,
        x_rotation: f64,
    ) {
        let (center, radii) = (center.into(), radii.into());
        let start_angle = self.current.map_or(0., |current| {
            // The current point's angle on the ellipse before rotation.
            let offset = current - center;
            let (sin, cos) = (-x_rotation).sin_cos();
            let x = offset.x * cos - offset.y * sin;
            let y = offset.x * sin + offset.y * cos;
            (y / radii.y).atan2(x / radii.x)
        });
        let arc = Arc::new(center, radii, start_angle, sweep, x_rotation);

        self.line_to(arc.start());
        for el in arc.append_iter(TOLERANCE) {
            self.path.push(el);
        }
        self.current = Some(arc.end());
    }

    /// Closes the current subpath.
    pub fn close_path(&mut self) {
        if self.subpath_open {
            self.path.close_path();
            self.subpath_open = false;
            self.current = self.subpath_start;
        }
    }

    /// Adds the rings of a polygon to the path as closed subpaths.
    ///
    /// The rings are oriented so that interior rings cut holes in the
    /// exterior under either fill rule, regardless of the polygon's winding
    /// order.
    pub fn polygon(&mut self, polygon: &Polygon<f64>) {
        self.ring(polygon.exterior(), true);
        for interior in polygon.interiors() {
            self.ring(interior, false);
        }
    }

    /// Rasterizes the region inside the path, then clears the path.
    pub fn fill(&mut self) -> Raster {
        let path = self.take_path();
        self.raster(&path, self.fill_rule)
    }

    /// Rasterizes the region within half the stroke width of the path, then
    /// clears the path.
    ///
    /// Open subpaths have butt caps. Corners have miter joins which fall back
    /// to bevel joins when the miter would extend beyond four times the stroke
    /// width.
    pub fn stroke(&mut self) -> Raster {
        let path = self.take_path();
        let style = Stroke::new(self.stroke_width)
            .with_join(Join::Miter)
            .with_caps(Cap::Butt);
        let outline = kurbo::stroke(path, &style, &StrokeOpts::default(), TOLERANCE);

        // Stroke outlines overlap themselves where the path turns or crosses
        // itself; the nonzero rule shades the union of those overlaps.
        self.raster(&outline, FillRule::NonZero)
    }

    fn raster(&self, path: &BezPath, fill_rule: FillRule) -> Raster {
        Raster::new(
            path_edges(path, TOLERANCE),
            fill_rule,
            self.sample_depth,
            self.width,
            self.height,
        )
    }

    fn take_path(&mut self) -> BezPath {
        self.current = None;
        self.subpath_start = None;
        self.subpath_open = false;
        std::mem::take(&mut self.path)
    }

    /// Ensures there is a subpath to extend. A subpath is started at the
    /// current point if the last one was closed, or at `fallback` if there is
    /// no current point. Returns whether there was a current point.
    fn continue_subpath(&mut self, fallback: Point) -> bool {
        match self.current {
            Some(current) if !self.subpath_open => {
                self.move_to(current);
                true
            }
            Some(_) => true,
            None => {
                self.move_to(fallback);
                false
            }
        }
    }

    fn ring(&mut self, ring: &LineString<f64>, positive: bool) {
        let points: Vec<Point> = ring.coords().map(|c| Point::new(c.x, c.y)).collect();
        if points.len() < 3 {
            return;
        }

        let doubled_area: f64 = points
            .iter()
            .zip(points.iter().cycle().skip(1))
            .map(|(a, b)| a.x * b.y - b.x * a.y)
            .sum();
        let mut points: Box<dyn Iterator<Item = Point>> = match (doubled_area > 0.) == positive {
            true => Box::new(points.into_iter()),
            false => Box::new(points.into_iter().rev()),
        };

        if let Some(first) = points.next() {
            self.move_to(first);
            points.for_each(|p| self.line_to(p));
            self.close_path();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::raster::{opacity_map, ShadeCommand};
    use float_eq::assert_float_eq;
    use geo_types::{coord, polygon, LineString};
    use std::f64::consts::PI;

    const WIDTH: u32 = 64;
    const HEIGHT: u32 = 64;

    fn canvas() -> Canvas {
        let mut canvas = Canvas::new(WIDTH, HEIGHT);
        canvas.set_sample_depth(SampleDepth::Super16);
        canvas
    }

    fn area(commands: impl Iterator<Item = ShadeCommand>) -> f64 {
        opacity_map(commands, WIDTH, HEIGHT)
            .iter()
            .map(|o| *o as f64)
            .sum()
    }

    fn rect(canvas: &mut Canvas, x: f64, y: f64, w: f64, h: f64) {
        canvas.move_to((x, y));
        canvas.line_to((x + w, y));
        canvas.line_to((x + w, y + h));
        canvas.line_to((x, y + h));
        canvas.close_path();
    }

    #[test]
    fn pixel_aligned_rect_is_exact() {
        let mut canvas = canvas();
        rect(&mut canvas, 2., 3., 10., 5.);
        let commands: Vec<ShadeCommand> = canvas.fill().collect();

        assert!(commands.iter().all(|c| match c {
            ShadeCommand::Boundary { coverage, .. } => *coverage == 1.,
            ShadeCommand::Span { .. } => true,
        }));
        assert_eq!(area(commands.into_iter()), 50.);
    }

    #[test]
    fn half_pixel_edges() {
        let mut canvas = canvas();
        rect(&mut canvas, 2.5, 2., 5., 2.);
        let map = opacity_map(canvas.fill(), WIDTH, HEIGHT);

        let row: Vec<f32> = map[2 * WIDTH as usize..][..9].to_vec();
        assert_eq!(row, vec![0., 0., 0.5, 1., 1., 1., 1., 0.5, 0.]);
    }

    #[test]
    fn circle_from_arc() {
        let mut canvas = canvas();
        canvas.move_to((52., 32.));
        canvas.arc((32., 32.), (20., 20.), 2. * PI, 0.);
        canvas.close_path();

        assert_float_eq!(area(canvas.fill()), PI * 400., r2nd <= 0.005);
    }

    #[test]
    fn arc_continues_from_current_point() {
        let mut canvas = canvas();
        // A half disc: the arc starts at the bottom of the circle and sweeps
        // through its left side to the top.
        canvas.move_to((32., 52.));
        canvas.arc((32., 32.), (20., 20.), PI, 0.);
        canvas.close_path();

        assert_float_eq!(area(canvas.fill()), PI * 200., r2nd <= 0.005);
    }

    #[test]
    fn curves() {
        let mut canvas = canvas();
        // A quadratic from (10, 40) through control (30, 0) to (50, 40) bounds a
        // parabolic segment of area 2/3 * base * height / 2.
        canvas.move_to((10., 40.));
        canvas.quadratic_to((30., 0.), (50., 40.));
        canvas.close_path();
        assert_float_eq!(area(canvas.fill()), 2. / 3. * 40. * 20., r2nd <= 0.005);

        // The same parabola as a cubic.
        canvas.move_to((10., 40.));
        canvas.cubic_to((70. / 3., 40. / 3.), (110. / 3., 40. / 3.), (50., 40.));
        canvas.close_path();
        assert_float_eq!(area(canvas.fill()), 2. / 3. * 40. * 20., r2nd <= 0.005);
    }

    #[test]
    fn fill_rules() {
        let overlapping_squares = |canvas: &mut Canvas| {
            rect(canvas, 10., 10., 20., 20.);
            rect(canvas, 20., 20., 20., 20.);
        };

        let mut canvas = canvas();
        overlapping_squares(&mut canvas);
        assert_eq!(area(canvas.fill()), 700.);

        canvas.set_fill_rule(FillRule::EvenOdd);
        overlapping_squares(&mut canvas);
        assert_eq!(area(canvas.fill()), 600.);
    }

    #[test]
    fn polygon_holes_regardless_of_orientation() {
        let exterior = LineString::from(vec![(10., 10.), (50., 10.), (50., 50.), (10., 50.)]);
        let hole = LineString::from(vec![(20., 20.), (30., 20.), (30., 30.), (20., 30.)]);
        let reversed_hole = LineString::from(hole.0.iter().rev().copied().collect::<Vec<_>>());

        for hole in [hole, reversed_hole] {
            let mut canvas = canvas();
            canvas.polygon(&Polygon::new(exterior.clone(), vec![hole]));
            assert_eq!(area(canvas.fill()), 1500.);
        }
    }

    #[test]
    fn polygon_macro() {
        let mut canvas = canvas();
        canvas.polygon(&polygon![
            coord! { x: 0., y: 0. },
            coord! { x: 8., y: 0. },
            coord! { x: 0., y: 8. },
        ]);
        // The diagonal passes exactly through some sample points.
        assert_float_eq!(area(canvas.fill()), 32., abs <= 1.);
    }

    #[test]
    fn stroke_line() {
        let mut canvas = canvas();
        canvas.set_stroke_width(4.);
        canvas.move_to((10., 20.));
        canvas.line_to((50., 20.));
        let map = opacity_map(canvas.stroke(), WIDTH, HEIGHT);

        assert_eq!(map.iter().map(|o| *o as f64).sum::<f64>(), 160.);
        for y in 18..22 {
            for x in 10..50 {
                assert_eq!(map[y * WIDTH as usize + x], 1.);
            }
        }
    }

    #[test]
    fn stroke_closed_square_has_mitered_corners() {
        let mut canvas = canvas();
        canvas.set_stroke_width(2.);
        rect(&mut canvas, 10., 10., 20., 20.);

        // 22 x 22 outer square minus 18 x 18 inner square.
        assert_eq!(area(canvas.stroke()), 160.);
    }

    #[test]
    fn self_crossing_stroke_shades_each_pixel_once() {
        let mut canvas = canvas();
        canvas.set_stroke_width(6.);
        canvas.move_to((10., 10.));
        canvas.cubic_to((70., 60.), (-10., 60.), (50., 10.));
        // `opacity_map` asserts each pixel is shaded at most once.
        let map = opacity_map(canvas.stroke(), WIDTH, HEIGHT);

        assert!(map.iter().all(|o| (0. ..=1.).contains(o)));
        assert!(map.contains(&1.));
    }

    #[test]
    fn clipped_to_bounds() {
        let mut canvas = Canvas::new(20, 10);
        rect(&mut canvas, -10., -10., 100., 100.);
        let map = opacity_map(canvas.fill(), 20, 10);

        assert!(map.iter().all(|o| *o == 1.));
    }

    #[test]
    fn fill_consumes_path() {
        let mut canvas = canvas();
        rect(&mut canvas, 2., 2., 4., 4.);
        assert_eq!(area(canvas.fill()), 16.);
        assert_eq!(canvas.fill().count(), 0);
    }

    #[test]
    fn subpath_continues_after_close() {
        let mut canvas = canvas();
        canvas.move_to((10., 10.));
        canvas.line_to((20., 10.));
        canvas.line_to((20., 20.));
        canvas.close_path();
        // Continues from (10, 10), the start of the closed subpath.
        canvas.line_to((0., 10.));
        canvas.line_to((0., 0.));
        assert_eq!(area(canvas.fill()), 100.);
    }

    #[test]
    fn line_to_without_move_to_starts_subpath() {
        let mut canvas = canvas();
        canvas.line_to((10., 10.));
        canvas.line_to((20., 10.));
        canvas.line_to((20., 20.));
        canvas.line_to((10., 20.));
        assert_eq!(area(canvas.fill()), 100.);
    }
}
