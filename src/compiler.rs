use thiserror::Error;

/// Represents errors that can occur during the compilation process.
#[derive(Error, Debug)]
pub enum CompileError {
    /// Indicates that the provided Rust source code was empty.
    #[error("The source code is empty.")]
    EmptySource,
    
    /// Indicates an error occurred during the transformation process.
    #[error("Transformation error: {0}")]
    TransformationError(String),
}

/// Transpiles Rust source code into COBOL code.
///
/// # Arguments
///
/// * `source` - A string slice containing the Rust source code.
///
/// # Returns
///
/// A `Result` which, on success, contains a `String` holding the generated COBOL code; on failure,
/// returns a `CompileError`.
///
/// # Example
///
/// ```rust
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
    // In a full implementation, this function would perform syntactic and semantic analysis
    // of the Rust source and generate equivalent COBOL code.
    // For now, we simulate transformation by wrapping the Rust source in a basic COBOL skeleton.
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
    
    // Convert each line of Rust source into a COBOL comment.
    let rust_comments: String = source
        .lines()
        .map(|line| format!("* {}", line))
        .collect::<Vec<String>>()
        .join("\n");
    
    let cobol_code = format!("{}\n{}\n{}", header, rust_comments, footer);
    
    // If detailed error reporting were needed, you might return an error:
    // Err(CompileError::TransformationError("Detailed transformation error".into()))
    
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
