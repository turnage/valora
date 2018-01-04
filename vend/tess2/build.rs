extern crate bindgen;

use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let tess2_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("libtess2");

    let config =
        format!("config={}_native",
                if &env::var("PROFILE").unwrap() == "release" { "release" } else { "debug" });

    Command::new("make")
        .arg(config)
        .current_dir(tess2_path.join("Build"))
        .output()
        .expect("libtess2 make");

    println!("cargo:rustc-link-lib=tess2");
    println!("cargo:rustc-link-search={}", tess2_path.join("Build").to_str().unwrap());

    let bindings = bindgen::Builder::default()
        .header("libtess2/Include/tesselator.h")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}