use regex::Regex;
use thiserror::Error;

/// Represents errors that can occur during the compilation process.
#[derive(Error, Debug)]
pub enum CompileError {
    /// The provided Rust source code was empty.
    #[error("The source code is empty.")]
    EmptySource,
    
    /// An error occurred during transformation.
    #[error("Transformation error: {0}")]
    TransformationError(String),
}

/// Transpiles Rust source code into COBOL code.
///
/// This function supports a very restricted subset of Rust:
/// - It recognizes the start of the main function (`fn main() {`)
/// - It converts simple `println!("...");` calls into COBOL `DISPLAY` statements.
/// - Other lines are emitted as comments (prefixed by an asterisk).
///
/// # Arguments
///
/// * `source` - A string slice containing Rust source code.
///
/// # Returns
///
/// A `Result` containing a `String` with the generated COBOL code, or a `CompileError` if the input is empty.
///
/// # Example
///
/// ```rust
/// use rust_to_cobol_compiler::compile;
///
/// let rust_code = r#"fn main() {
///     println!("Hello, world!");
/// }"#;
/// let cobol = compile(rust_code).expect("Compilation failed");
/// assert!(cobol.contains("DISPLAY \"Hello, world!\"."));
/// ```
pub fn compile(source: &str) -> Result<String, CompileError> {
    if source.trim().is_empty() {
        return Err(CompileError::EmptySource);
    }
    
    // Define regex patterns for recognized Rust constructs.
    let re_main = Regex::new(r"^\s*fn\s+main\s*\(\s*\)\s*{").unwrap();
    let re_println = Regex::new(r#"println!\s*\(\s*"(.*?)"\s*\)\s*;"#).unwrap();
    let re_closing_brace = Regex::new(r"^\s*}\s*$").unwrap();
    
    // COBOL header.
    let mut cobol_lines = vec![
        "IDENTIFICATION DIVISION.",
        "PROGRAM-ID. GENERATED.",
        "AUTHOR. Auto-Generated.",
        "DATE-WRITTEN. 2025.",
        "ENVIRONMENT DIVISION.",
        "INPUT-OUTPUT SECTION.",
        "FILE-CONTROL.",
        "DATA DIVISION.",
        "WORKING-STORAGE SECTION.",
        "PROCEDURE DIVISION.",
    ];
    
    // Process each line of the Rust source.
    for line in source.lines() {
        if re_main.is_match(line) {
            // Start the main paragraph.
            cobol_lines.push("MAIN-PARA.");
        } else if let Some(caps) = re_println.captures(line) {
            // Transform println! into a COBOL DISPLAY statement.
            let content = caps.get(1).unwrap().as_str();
            cobol_lines.push(&format!("DISPLAY \"{}\".", content));
        } else if re_closing_brace.is_match(line) {
            // Ignore closing braces.
            continue;
        } else {
            // For any other line, output it as a comment.
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                cobol_lines.push(&format!("* {}", trimmed));
            }
        }
    }
    
    // COBOL footer.
    cobol_lines.push("GOBACK.");
    cobol_lines.push("STOP RUN.");
    
    Ok(cobol_lines.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_with_valid_source() {
        let rust_code = r#"fn main() {
    println!("Hello, world!");
    let x = 42;
}"#;
        let result = compile(rust_code);
        assert!(result.is_ok());
        let cobol = result.unwrap();
        // Check that the header and transformed println exist.
        assert!(cobol.contains("IDENTIFICATION DIVISION."));
        assert!(cobol.contains("MAIN-PARA."));
        assert!(cobol.contains("DISPLAY \"Hello, world!\"."));
        // The unrecognized line is output as a comment.
        assert!(cobol.contains("* let x = 42;"));
    }

    #[test]
    fn test_compile_with_empty_source() {
        let rust_code = "   ";
        let result = compile(rust_code);
        assert!(matches!(result, Err(CompileError::EmptySource)));
    }
}
