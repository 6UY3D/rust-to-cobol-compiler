#![doc = "Rust-to-COBOL compiler that leverages rustc's internal parser and full language support."]

pub mod frontend_rustc;
pub mod mir_lowering;
pub mod monomorphizer;
pub mod trait_resolution;
pub mod closure_transform;
pub mod async_transform;
pub mod macro_expansion;
pub mod lifetime_erasure;
pub mod backend;
pub mod ir;
pub mod transpile;

pub use transpile::{compile_full_rustc, CompileError};
