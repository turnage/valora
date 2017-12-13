use geom::Point;
use palette::Colora;

pub enum Shader {
    Constant(Colora),
    Linear(Box<Fn(Point) -> Colora>),
    Empty,
}

impl Shader {
    pub fn constant(color: Colora) -> Shader { Shader::Constant(color) }

    pub fn linear<F: 'static + Fn(Point) -> Colora>(f: F) -> Shader { Shader::Linear(Box::new(f)) }

    pub fn empty() -> Shader { Shader::Empty }

    pub fn shade(&self, point: Point) -> Colora {
        match *self {
            Shader::Constant(color) => color.clone(),
            Shader::Linear(ref f) => f(point),
            Shader::Empty => Colora::rgb(0.0, 0.0, 0.0, 0.0),
        }
    }
}
