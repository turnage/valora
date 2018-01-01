pub trait SubdivideEdges: Sized {
    // Cuts the edges of the geometry in half so that points exist at the
    // midpoint of all previously existing edges. The actual shape should
    // not change.
    fn subdivide_edges(self) -> Self;
}