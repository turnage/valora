use geom::{Place, Point, Poly};
use std::rc::Rc;

pub trait SpawnSrc {
    fn spawn_points(&self) -> Vec<Point>;
}

impl<P: Poly> SpawnSrc for P {
    default fn spawn_points(&self) -> Vec<Point> { self.vertices() }
}

impl SpawnSrc for Vec<Point> {
    fn spawn_points(&self) -> Vec<Point> { self.clone() }
}

pub trait Spawner<G> {
    fn spawn(&self, point: Point, index: usize) -> G;
}

#[derive(Clone)]
pub struct Instancer<S, G> {
    src: S,
    f: Rc<Fn(&S, Point, usize) -> G>,
}

impl<G: Clone + Place> From<G> for Instancer<G, G> {
    fn from(src: G) -> Self {
        Self { src, f: Rc::new(|src, point, _index| src.clone().place(point)) }
    }
}

impl<S, G> Instancer<S, G> {
    pub fn with<F: 'static + Fn(&S, Point, usize) -> G>(src: S, f: F) -> Self {
        Self { src, f: Rc::new(f) }
    }
}

impl<S, G> Spawner<G> for Instancer<S, G> {
    fn spawn(&self, point: Point, index: usize) -> G { (self.f)(&self.src, point, index) }
}

pub fn spawn<G, Src, S>(s: &S, src: &Src) -> Vec<G>
    where Src: SpawnSrc,
          S: Spawner<G>
{
    src.spawn_points()
        .iter()
        .enumerate()
        .map(|(i, p)| s.spawn(*p, i))
        .collect()
}