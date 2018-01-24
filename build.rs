extern crate glossy_codegen as glsl;

fn main() {
    println!("cargo:rerun-if-changed=shaders/*");
    glsl::Config::new(glsl::Language::OpenGlEs30)
        .vertex("shaders/*.vert")
        .fragment("shaders/*.frag")
        .include("shaders/include/*")
        .discard_line_info()
        .build();
}
