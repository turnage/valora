use palette::Colora;
use geom::Point;

pub enum Shader {
    Constant(Colora),
}

impl Shader {
    pub fn constant(color: Colora) -> Shader {
        Shader::Constant(color)
    }

    pub fn shade(&self, _point: Point) -> Colora {
        match *self {
            Shader::Constant(color) => color.clone(),
        }
    }
}
