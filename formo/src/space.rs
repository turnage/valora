//! Spatial definitions.

use nalgebra::{base::*, Matrix};

pub type V2 = Matrix<f32, U2, U1, ArrayStorage<f32, U2, U1>>;
pub type V3 = Matrix<f32, U3, U1, ArrayStorage<f32, U3, U1>>;
pub type V4 = Matrix<f32, U4, U1, ArrayStorage<f32, U4, U1>>;
