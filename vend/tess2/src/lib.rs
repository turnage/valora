#![allow()]

extern crate itertools;

pub mod safe;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));