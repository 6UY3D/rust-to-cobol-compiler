use clap::Parser;
use std::{fs, process};

use rust_to_cobol_compiler::{compile, CompileError};

/// Transpiles Rust source code to COBOL.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to the input Rust source file.
    input: String,

    /// Optional path to the output file; if omitted, output is printed to stdout.
    #[arg(short, long)]
    output: Option<String>,
}

fn main() {
    let cli = Cli::parse();

    // Read the input file.
    let source = match fs::read_to_string(&cli.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", cli.input, e);
            process::exit(1);
        }
    };

    // Perform the compilation.
    match compile(&source) {
        Ok(cobol_code) => {
            if let Some(output_path) = cli.output {
                if let Err(e) = fs::write(&output_path, cobol_code) {
                    eprintln!("Error writing to file '{}': {}", output_path, e);
                    process::exit(1);
                }
            } else {
                println!("{}", cobol_code);
            }
        }
        Err(CompileError::EmptySource) => {
            eprintln!("Compilation failed: input source is empty.");
            process::exit(1);
        }
        Err(CompileError::TransformationError(msg)) => {
            eprintln!("Compilation failed: {}", msg);
            process::exit(1);
        }
    }
}
