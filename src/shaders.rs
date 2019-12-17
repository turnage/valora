//! Fragment shader API.

use crate::{
    gpu::{Gpu, Shader},
    uniforms::OwnedUniforms,
    Result,
};
use glium::Program;
use glslwatch::GLSLTree;
use std::{path::Path, rc::Rc};

/// A dynamically reloaded GLSL program.
pub struct ShaderProgram {
    tree: GLSLTree,
    gpu: Gpu,
    last_program: Option<Rc<Program>>,
    last_shader: Shader,
}

impl ShaderProgram {
    /// Creates a dynamically reloaded glsl program from the given path.
    pub fn new(gpu: &Gpu, glsl: impl AsRef<Path>) -> Result<Self> {
        let include_paths: [String; 0] = [];
        Self::new_with_include_paths(gpu, glsl, &include_paths)
    }

    /// Creates a new dynamically reloaded glsl program from the given path, searching the given
    /// directories for `#include` paths.
    pub fn new_with_include_paths<S: AsRef<Path>>(
        gpu: &Gpu,
        glsl: impl AsRef<Path>,
        include_directories: &[S],
    ) -> Result<Self> {
        Ok(Self {
            gpu: gpu.clone(),
            tree: GLSLTree::new(glsl, include_directories)?,
            last_shader: gpu.default_shader(),
            last_program: None,
        })
    }

    /// Create a usable shader by binding uniform values.
    pub fn bind(&mut self, uniforms: impl OwnedUniforms + 'static) -> Shader {
        match self.try_bind(uniforms) {
            Ok(shader) => shader,
            Err(e) => {
                eprintln!("Failed to load custom shader; error: {:?}", e);
                self.last_shader.clone()
            }
        }
    }

    pub fn try_bind(&mut self, uniforms: impl OwnedUniforms + 'static) -> Result<Shader> {
        let program = if self.tree.expired()? {
            let mut new_tree = self.tree.clone().refresh()?;
            std::mem::swap(&mut self.tree, &mut new_tree);
            let src = self.tree.render();
            let program = self.gpu.compile_glsl(src)?;
            self.last_program = Some(program.clone());
            program
        } else if let Some(program) = self.last_program.as_ref() {
            program.clone()
        } else {
            let src = self.tree.render();
            let program = self.gpu.compile_glsl(src)?;
            self.last_program = Some(program.clone());
            program
        };

        self.last_shader = self.gpu.build_shader(program, uniforms);
        Ok(self.last_shader.clone())
    }
}
