extern crate bindgen;

use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    Command::new("make")
        .current_dir(PathBuf::from("libtess2").join("Build"))
        .output()
        .expect("libtess2 make");

    println!("cargo:rustc-link-lib=tess2");
    println!("cargo:rustc-link-search=libtess2/Build");

    let bindings = bindgen::Builder::default()
        .header("libtess2/Include/tesselator.h")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}