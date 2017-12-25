use geom::{Place, Point, Poly};
use std::rc::Rc;

pub trait SpawnSrc {
    fn spawn_points<'a>(&'a self) -> &'a [Point];
}

impl<P: Poly> SpawnSrc for P {
    fn spawn_points<'a>(&'a self) -> &'a [Point] { self.vertices() }
}

pub trait Spawner<G> {
    fn spawn(&self, point: Point, index: usize) -> G;
}

#[derive(Clone)]
pub struct Instancer<G: Place + Clone> {
    src: G,
    f: Option<Rc<Fn(&G, Point, usize) -> G>>,
}

impl<G: Place + Clone> Instancer<G> {
    pub fn new(src: G) -> Self { Self { src, f: None } }
}

impl<G: Place + Clone> Spawner<G> for Instancer<G> {
    fn spawn(&self, point: Point, index: usize) -> G {
        let instance = self.src.clone().place(point);
        match self.f {
            Some(ref f) => {
                let f = f.as_ref();
                f(&instance, point, index)
            }
            None => instance,
        }
    }
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