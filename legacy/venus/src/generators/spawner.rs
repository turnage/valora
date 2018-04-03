use poly::{Point, Poly};
use rand::StdRng;

pub trait SpawnSrc {
    fn spawn_points(&self) -> Vec<Point>;
}

impl SpawnSrc for Poly {
    fn spawn_points(&self) -> Vec<Point> {
        self.vertices()
    }
}

impl<P: Clone + Into<Poly>> SpawnSrc for P {
    default fn spawn_points(&self) -> Vec<Point> {
        self.clone().into().vertices()
    }
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

impl Spawner<Poly> for Poly {
    fn spawn(&self, cfg: SpawnCfg) -> Poly {
        self.clone().place(cfg.point)
    }
}

impl<P: Clone + Into<Poly>> Spawner<Poly> for P {
    default fn spawn(&self, cfg: SpawnCfg) -> Poly {
        self.clone().into().place(cfg.point)
    }
}

pub fn spawn<G, Src, S>(s: &S, src: &Src, mut rng: StdRng) -> Vec<G>
where
    Src: SpawnSrc,
    S: Spawner<G>,
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

pub fn generate<G, S: SpawnSrc + Spawner<G>>(s: &S, rng: StdRng) -> Vec<G> {
    spawn(s, s, rng)
}
