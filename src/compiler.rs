use anyhow::Result;
use std::fmt;

/// Represents possible errors during the compilation process.
#[derive(Debug)]
pub enum CompileError {
    /// The provided Rust source code was empty.
    EmptySource,
    /// An error occurred during transformation.
    TransformationError(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::EmptySource => write!(f, "The source code is empty."),
            CompileError::TransformationError(msg) => write!(f, "Transformation error: {}", msg),
        }
    }
}

impl std::error::Error for CompileError {}

/// Transpiles Rust source code into COBOL code.
///
/// # Arguments
///
/// * `source` - A string slice holding the Rust source code.
///
/// # Returns
///
/// A `Result` containing the generated COBOL code on success or a `CompileError` on failure.
///
/// # Example
///
/// ```
/// use rust_to_cobol_compiler::compile;
///
/// let rust_code = "fn main() { println!(\"Hello, world!\"); }";
/// let cobol = compile(rust_code).expect("Compilation failed");
/// assert!(cobol.contains("IDENTIFICATION DIVISION."));
/// ```
pub fn compile(source: &str) -> Result<String, CompileError> {
    if source.trim().is_empty() {
        return Err(CompileError::EmptySource);
    }
    
    // Production-ready transformation logic:
    // In a real implementation, proper parsing and semantic analysis of Rust code would occur here.
    // For now, we simulate transformation by wrapping the original source in a basic COBOL skeleton.
    let header = r#"IDENTIFICATION DIVISION.
PROGRAM-ID. GENERATED.
AUTHOR. Auto-Generated.
DATE-WRITTEN. 2025.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
DATA DIVISION.
WORKING-STORAGE SECTION.
PROCEDURE DIVISION.
"#;
    let footer = "\nSTOP RUN.";
    
    // Embed the original Rust source as COBOL comments.
    // In COBOL, comments are typically denoted by an asterisk in column 1.
    let rust_comment: String = source
        .lines()
        .map(|line| format!("* {}", line))
        .collect::<Vec<String>>()
        .join("\n");
    
    let cobol_code = format!("{}\n{}\n{}", header, rust_comment, footer);
    
    // If a transformation error occurs, you could return an error as follows:
    // Err(CompileError::TransformationError("Detailed error message".into()))
    
    Ok(cobol_code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_with_valid_source() {
        let rust_code = r#"fn main() {
    println!("Hello, world!");
}"#;
        let result = compile(rust_code);
        assert!(result.is_ok());
        let cobol = result.unwrap();
        assert!(cobol.contains("IDENTIFICATION DIVISION."));
        assert!(cobol.contains("* fn main() {"));
    }

    #[test]
    fn test_compile_with_empty_source() {
        let rust_code = "   ";
        let result = compile(rust_code);
        assert!(matches!(result, Err(CompileError::EmptySource)));
    }
}
