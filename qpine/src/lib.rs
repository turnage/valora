//! A quad tree with support for custom heuristics.

use formo::Rect;

/// A heuristic to apply when performing a nearest-neighbor search of a quad tree.
pub trait Heuristic<V> {
    /// Returns the distance between two leaves to consider when performing a nearest-neighbor search.
    fn distance(&self, leaf_a: &Leaf<V>, leaf_b: &Leaf<V>) -> f32;

    /// Returns true if the quad should be searched in a nearest-neighbor search, given the query and current champion.
    fn predicate(&self, query: &Leaf<V>, champion: &Leaf<V>, candidate: Quad<V>) -> bool;
}

/// A rule to set the tag leaf of a quad when inserting a new leaf into a quad tree.
pub trait Tagger<V> {
    /// Returns the chosen tag leaf for the quad, between the current tag leaf or the newly inserted leaf.
    fn update_quad_tag<'a>(
        &'a self,
        current_tag: Option<&'a Leaf<V>>,
        new_leaf: &'a Leaf<V>,
    ) -> Option<&'a Leaf<V>>;
}

pub struct QuadTree<V, T> {
    tagger: T,
    root: Quad<V>,
}

impl<V, T: Tagger<V>> QuadTree<V, T> {
    pub fn new(tagger: T, root: Rect) -> Self {
        Self {
            tagger,
            root: Quad {
                tag: None,
                rect: root,
            },
        }
    }
}

pub struct Leaf<V> {
    v: V,
}

pub struct Quad<V> {
    tag: Option<Leaf<V>>,
    rect: Rect,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
