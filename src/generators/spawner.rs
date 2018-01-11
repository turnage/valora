use poly::{Point, Poly};
use rand::StdRng;
use std::rc::Rc;
use transforms::Place;

pub trait SpawnSrc {
    fn spawn_points(&self) -> Vec<Point>;
}

impl<P: Poly> SpawnSrc for P {
    default fn spawn_points(&self) -> Vec<Point> { self.vertices() }
}

pub struct SpawnCfg<'a> {
    pub point: Point,
    pub index: usize,
    pub n: usize,
    pub percent: f32,
    pub rng: &'a mut StdRng,
}

pub trait Spawner<G> {
    fn spawn(&self, cfg: SpawnCfg) -> G;
}

#[derive(Clone)]
pub struct Instancer<S, G> {
    src: S,
    f: Rc<Fn(&S, SpawnCfg) -> G>,
}

impl<G: Clone + Place> From<G> for Instancer<G, G> {
    fn from(src: G) -> Self {
        Self { src, f: Rc::new(|src, cfg: SpawnCfg| src.clone().place(cfg.point)) }
    }
}

impl<S, G> Instancer<S, G> {
    pub fn with<F: 'static + Fn(&S, SpawnCfg) -> G>(src: S, f: F) -> Self {
        Self { src, f: Rc::new(f) }
    }
}

impl<S, G> Spawner<G> for Instancer<S, G> {
    fn spawn(&self, cfg: SpawnCfg) -> G { (self.f)(&self.src, cfg) }
}

pub fn spawn<G, Src, S>(s: &S, src: &Src, mut rng: StdRng) -> Vec<G>
    where Src: SpawnSrc,
          S: Spawner<G>
{
    let spawn_points = src.spawn_points();
    let n = spawn_points.len();
    spawn_points
        .iter()
        .enumerate()
        .map(|(i, p)| {
            s.spawn(SpawnCfg {
                        point: *p,
                        index: i,
                        n,
                        percent: (i as f32) / (n as f32),
                        rng: &mut rng,
                    })
        })
        .collect()
}

pub fn generate<G, S: SpawnSrc + Spawner<G>>(s: &S, rng: StdRng) -> Vec<G> { spawn(s, s, rng) }
