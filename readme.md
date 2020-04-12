# valora

[![](https://docs.rs/valora/badge.svg)](https://docs.rs/valora) [![crates.io](https://img.shields.io/crates/v/valora.svg)](https://crates.io/crates/valora) ![Rust](https://github.com/turnage/valora/workflows/Rust/badge.svg?branch=master)

A brush for generative fine art. [Read the guide!](https://paytonturnage.gitbook.io/valora/)

This a graphics library and CLI focused on generative fine art for print.

Features

* Repeatable works at arbitrary resolutions without changing the work
* Managed rngs for repeatable works and controlled rng trees
* Support for using a different, custom GLSL shader for each vector path
* GLSL live coding with "#include" support
* An ergonomic derive-based GLSL uniforms interface
* Animation support for brainstorming and cumulative pieces

![](https://i.imgur.com/e2rsMVb.png)
