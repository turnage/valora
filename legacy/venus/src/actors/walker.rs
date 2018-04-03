use petgraph::prelude::*;
use poly::Point;
use rand::StdRng;

pub struct Walker {
    walk: Graph<Point, ()>,
    leaves: Vec<NodeIndex<u32>>,
    heuristic: WalkHeuristic,
}

pub enum WalkHeuristic {
    Greedy(Box<Fn(Point, &mut StdRng) -> Vec<Point>>),
}

impl Walker {
    pub fn new(root: Point, heuristic: WalkHeuristic) -> Self {
        let (walk, leaves) = {
            let mut g = Graph::new();
            let index = g.add_node(root);
            (g, vec![index])
        };
        Self {
            walk,
            leaves,
            heuristic,
        }
    }

    pub fn edges(&self) -> Vec<(Point, Point)> {
        self.walk
            .edge_references()
            .map(|er| (self.walk[er.source()], self.walk[er.target()]))
            .collect()
    }

    pub fn walk(self, depth: usize, rng: &mut StdRng) -> Self {
        (0..depth).into_iter().fold(self, |mut acc, _| {
            acc.step(rng);
            acc
        })
    }

    pub fn step(&mut self, rng: &mut StdRng) -> Vec<(Point, Point)> {
        let mut vpairs = Vec::new();
        let updates: Vec<(NodeIndex, Vec<Point>)> = self.leaves
            .iter()
            .map(|leaf| (*leaf, self.step_from_strand(self.walk[*leaf], rng)))
            .collect();
        self.leaves.clear();
        for (anchor, new_leaves) in updates.into_iter() {
            let edges: Vec<(NodeIndex, NodeIndex)> = new_leaves
                .into_iter()
                .map(|leaf| (anchor, self.walk.add_node(leaf)))
                .collect();
            for (anchor, leaf) in edges {
                vpairs.push((self.walk[anchor], self.walk[leaf]));
                self.walk.add_edge(anchor, leaf, ());
                self.leaves.push(leaf);
            }
        }
        vpairs
    }

    fn step_from_strand(&self, leaf: Point, rng: &mut StdRng) -> Vec<Point> {
        match self.heuristic {
            WalkHeuristic::Greedy(ref f) => f(leaf, rng),
        }
    }
}
