pub fn iterate<S, F: Fn(S) -> S>(f: F, s: S, n: usize) -> S {
    (0..n).into_iter().fold(s, |s, _| f(s))
}