//! The browser interface for valora: an egui app which shows the frames a
//! valora server renders, and sends it commands.

pub mod protocol;

#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
mod preview;
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
mod settings;

#[cfg(target_arch = "wasm32")]
mod app;
#[cfg(not(target_arch = "wasm32"))]
pub mod assets;

#[cfg(target_arch = "wasm32")]
pub use app::App;
