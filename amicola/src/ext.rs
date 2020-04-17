//! Extensions to the standard library for this crate.

/// Returns (min, max) by comparing `a` and `b`.
pub fn min_max<T: PartialOrd>(a: T, b: T) -> (T, T) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

/// Returns (min, max) by comparing `a` and `b` with a key produced by `f`.
pub fn min_max_by<T, K: PartialOrd>(a: T, b: T, f: impl Fn(&T) -> K) -> (T, T) {
    if f(&a) < f(&b) {
        (a, b)
    } else {
        (b, a)
    }
}
