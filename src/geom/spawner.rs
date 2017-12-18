use geom::{Geometry, Point, Poly, Translate};

pub trait SpawnSrc {
    fn spawn_points<'a>(&'a self) -> &'a [Point];
}

impl<P: Poly> SpawnSrc for P {
    fn spawn_points<'a>(&'a self) -> &'a [Point] { self.vertices() }
}

pub trait Spawner<G: Geometry> {
    fn spawn(&self, point: Point, index: usize) -> G;
}

pub struct Instancer<G: Geometry + Translate> {
    src: G,
    f: Option<Box<Fn(&G, Point, usize) -> G>>,
}

impl<G: Geometry + Translate> Instancer<G> {
    pub fn new(src: G) -> Self { Self { src, f: None } }
}

impl<G: Geometry + Translate> Spawner<G> for Instancer<G> {
    fn spawn(&self, point: Point, index: usize) -> G {
        let instance = self.src.clone().translate_to(point);
        match self.f {
            Some(ref f) => {
                let f = f.as_ref();
                f(&instance, point, index)
            }
            None => instance,
        }
    }
}

pub fn spawn<G, Src>(s: &Spawner<G>, src: &Src) -> Vec<G>
    where G: Geometry,
          Src: SpawnSrc
{
    src.spawn_points()
        .iter()
        .enumerate()
        .map(|(i, p)| s.spawn(*p, i))
        .collect()
}