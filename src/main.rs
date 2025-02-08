use std::env;
use std::fs;
use std::process;

// Import the compile function and CompileError from your library.
// Make sure your lib.rs re-exports these items (or adjust the path as necessary).
use rust_to_cobol_compiler::{compile, CompileError};

fn main() {
    // Collect command-line arguments.
    // We expect the user to provide exactly one argument: the source file path.
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        process::exit(1);
    }
    let filename = &args[1];

    // Attempt to read the content of the provided file.
    let source = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        process::exit(1);
    });

    // Use the compile function to convert the Rust source code to COBOL.
    match compile(&source) {
        Ok(cobol_code) => {
            println!("Generated COBOL Code:\n\n{}", cobol_code);
        }
        Err(err) => {
            eprintln!("Compilation error: {:?}", err);
            process::exit(1);
        }
    }
}
