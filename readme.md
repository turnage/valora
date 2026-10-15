# valora

[![](https://docs.rs/valora/badge.svg)](https://docs.rs/valora) [![crates.io](https://img.shields.io/crates/v/valora.svg)](https://crates.io/crates/valora) ![Rust](https://github.com/turnage/valora/workflows/Rust/badge.svg?branch=master)

A brush for generative fine art. [Read the guide!](https://paytonturnage.gitbook.io/valora/)

This a graphics library and CLI focused on generative fine art for print.

Features

* Repeatable works at arbitrary resolutions without changing the work
* Managed rngs for repeatable works and controlled rng trees
* Support for using a different, custom WGSL shader for each vector path
* WGSL live coding
* An ergonomic derive-based shader uniforms interface
* Animation support for brainstorming and cumulative pieces
* A live preview in the browser, at http://127.0.0.1:6161 by default

The browser preview is an egui app in `valora_web`. After changing it, rebuild
the page the server embeds with `valora_web/build.sh` (requires
[trunk](https://github.com/trunk-rs/trunk)).

![](https://i.imgur.com/e2rsMVb.png)
