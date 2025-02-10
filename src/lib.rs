#![doc = "A production-ready transpiler that converts Rust source code into COBOL."]

pub mod compiler;

pub use compiler::{compile, CompileError};
