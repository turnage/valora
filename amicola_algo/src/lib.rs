mod arena;

use amicola_types::*;
use crate::arena::*;

#[derive(Debug)]
pub enum Error {
    PointNotContainedInTreeRegion {
        point_to_insert: Point,
        region: Region,
    }
}

/// A tree for efficiently searching points and regions in 2D space.
pub struct QuadTree {
    root: Quadrant,
    store: Arena<Node>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum QuadrantName {
    I,
    II,
    III,
    IV
}

impl QuadrantName {
    fn as_child_index(self) -> usize {
        match self {
            QuadrantName::I => 0,
            QuadrantName::II => 1,
            QuadrantName::III => 2,
            QuadrantName::IV => 3,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Region {
    bottom_left: Point,
    width: f32,
    height: f32,
}

impl Region {
    fn containing_quadrant(&self, p: Point) -> Option<QuadrantName> {
        if p.x < self.bottom_left.x || p.x > self.bottom_left.x + self.width || p.y < self.bottom_left.y || p.y > self.bottom_left.y + self.height {
            return None;
        }

        Some(if p.x >= self.bottom_left.x + (self.width/2.0) {
            if p.y >= self.bottom_left.y + (self.height/2.0) {
                QuadrantName::I
            } else {
                QuadrantName::IV
            }
        } else {
            if p.y >= self.bottom_left.y + (self.height/2.0) {
                QuadrantName::II
            } else {
                QuadrantName::III
            }
        })
    }

    fn subregion(&self, quadrant: QuadrantName) -> Region {
        Region {
            bottom_left: match quadrant {
                QuadrantName::I => Point {
                        x: self.bottom_left.x + self.width / 2.0,
                        y: self.bottom_left.y + self.height / 2.0,
                },
                QuadrantName::II => Point {
                    x: self.bottom_left.x,
                    y: self.bottom_left.y + self.height / 2.0,
                },
                QuadrantName::III => self.bottom_left,
                QuadrantName::IV => Point {
                    x: self.bottom_left.x + self.width / 2.0,
                    y: self.bottom_left.y
                }
            },
            width: self.width / 2.0,
            height: self.height / 2.0,
        }
    }
}

struct Quadrant {
    region: Region,
    children: [Option<ItemId>; 4],
}

struct Leaf {
    end: Point,
    region: Region,
}

enum Node {
    Quadrant(Quadrant),
    Leaf(Leaf),
}

impl Node {
    fn insert(&mut self, store: &mut Arena<Node>, p: Point) -> Result<(), Error> {
        match self {
            Node::Quadrant(q) => {
                let quadrant = q.region.containing_quadrant(p).ok_or(Error::PointNotContainedInTreeRegion {
                    point_to_insert: p,
                    region: q.region,
                })?;
                if let Some(child) = q.children[quadrant.as_child_index()].and_then(|id| store.get_mut(id)) {
                    child.insert(store, p)?;
                } else {
                    let id = store.insert(Node::Leaf(Leaf { end: p, region: q.region.subregion(quadrant) }));
                    q.children[quadrant.as_child_index()] = Some(id);
                }
            }
            Node::Leaf(l) => {
                *self = Node::Quadrant(Quadrant {
                    region: l.region,
                    children: [None; 4],
                });
                self.insert(store, l.end)?;
                self.insert(store, p)?;
            }
        };
        
        Ok(())
    }
}

impl QuadTree {
    pub fn new(region: Region) -> Self {
        Self {
            root: Quadrant {
                region,
                children: [None; 4],
            },
            store: Arena::new()
        }
    }

    pub fn insert(&mut self, p: Point) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
