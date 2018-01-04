#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate itertools;

pub mod safe;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));