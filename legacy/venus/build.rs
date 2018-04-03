extern crate glob;
extern crate glossy_codegen as glsl;

fn main() {
    for entry in glob::glob(&format!("{}/{}", env!("CARGO_MANIFEST_DIR"), "shaders/*"))
        .expect("compiled glob")
    {
        match entry {
            Ok(path) => println!("cargo:rerun-if-changed={}", path.display()),
            _ => {}
        }
    }

    glsl::Config::new(glsl::Language::OpenGlEs30)
        .vertex("shaders/*.vert")
        .fragment("shaders/*.frag")
        .include("shaders/include/*")
        .build();
}
