//! The built page, embedded for the server. Rebuild with `./build.sh`.

pub const INDEX_HTML: &str = include_str!("../dist/index.html");
pub const JS: &str = include_str!("../dist/valora_web.js");
pub const WASM: &[u8] = include_bytes!("../dist/valora_web_bg.wasm");
