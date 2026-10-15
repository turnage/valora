//! Fragment shader API.

use crate::{
    gpu::{Gpu, Shader},
    uniforms::UniformSet,
    Result,
};
use anyhow::Context;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};

/// A WGSL shader which is reloaded when its file changes.
///
/// The file defines a function which returns the linear RGBA color of each
/// fragment of the paths it shades:
///
/// ```wgsl
/// fn shade(fragment: Fragment) -> vec4<f32> {
///     return fragment.color;
/// }
/// ```
///
/// Valora declares `Fragment`, the `valora` uniform with the output size,
/// scale, and time, and `world_position(fragment)`; see
/// `src/shaders/prelude.wgsl`. Uniforms bound with [`ShaderProgram::bind`] are
/// available at `@group(1) @binding(0)`.
pub struct ShaderProgram {
    gpu: Gpu,
    path: PathBuf,
    modified: Option<SystemTime>,
    pipeline: Arc<wgpu::RenderPipeline>,
}

impl ShaderProgram {
    /// Compiles the WGSL shader at the given path.
    pub fn new(gpu: &Gpu, path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref().to_path_buf();
        let modified = modified(&path);
        let pipeline = compile(gpu, &path)?;
        Ok(Self {
            gpu: gpu.clone(),
            path,
            modified,
            pipeline,
        })
    }

    /// Create a usable shader by binding uniform values.
    ///
    /// If the shader's file has changed and no longer compiles, the error is
    /// printed and the last version which compiled is used.
    pub fn bind(&mut self, uniforms: impl UniformSet) -> Shader {
        if let Err(e) = self.reload() {
            eprintln!("Failed to reload shader; error: {:?}", e);
        }
        self.gpu.build_shader(self.pipeline.clone(), &uniforms)
    }

    /// Like `bind`, but returns the error if the shader's file has changed and
    /// no longer compiles.
    pub fn try_bind(&mut self, uniforms: impl UniformSet) -> Result<Shader> {
        self.reload()?;
        Ok(self.gpu.build_shader(self.pipeline.clone(), &uniforms))
    }

    fn reload(&mut self) -> Result<()> {
        let modified = modified(&self.path);
        if modified == self.modified {
            return Ok(());
        }

        // Only retry a broken file once it changes again.
        self.modified = modified;
        self.pipeline = compile(&self.gpu, &self.path)?;
        Ok(())
    }
}

fn modified(path: &Path) -> Option<SystemTime> {
    std::fs::metadata(path).and_then(|m| m.modified()).ok()
}

fn compile(gpu: &Gpu, path: &Path) -> Result<Arc<wgpu::RenderPipeline>> {
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("reading shader {}", path.display()))?;
    gpu.compile(&source)
        .with_context(|| format!("compiling shader {}", path.display()))
}
