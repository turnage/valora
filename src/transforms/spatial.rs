use geom::Point;

pub trait Percent: Sized {
    fn percent(self, percent: f32) -> Self;
}

pub trait Place: Sized {
    fn place(self, dest: Point) -> Self;
}

pub trait Translate: Sized {
    fn translate(self, delta: Point) -> Self;
}

pub trait Scale {
    fn scale(self, scale: f32) -> Self;
}