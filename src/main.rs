use clap::Parser;
use log::{debug, error, info};
use std::{fs, io::{self, Read}, process};

use rust_to_cobol_compiler::{compile, CompileError};

/// Transpiles Rust source code to COBOL.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to the input Rust source file. Use "-" to read from STDIN.
    #[arg(default_value = "-")]
    input: String,

    /// Optional path to the output file; if omitted, output is printed to stdout.
    #[arg(short, long)]
    output: Option<String>,

    /// Increase verbosity (e.g., -v, -vv).
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

fn main() {
    // Initialize logging (env_logger respects the RUST_LOG environment variable).
    env_logger::init();

    let cli = Cli::parse();

    // Read source from file or STDIN.
    let source = if cli.input == "-" {
        info!("Reading source from STDIN");
        let mut buffer = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut buffer) {
            error!("Error reading from STDIN: {}", e);
            process::exit(1);
        }
        buffer
    } else {
        info!("Reading source from file: {}", cli.input);
        match fs::read_to_string(&cli.input) {
            Ok(s) => s,
            Err(e) => {
                error!("Error reading file '{}': {}", cli.input, e);
                process::exit(1);
            }
        }
    };

    debug!("Source length: {} characters", source.len());

    // Attempt to compile Rust source to COBOL.
    match compile(&source) {
        Ok(cobol_code) => {
            if let Some(output_path) = cli.output {
                info!("Writing output to file: {}", output_path);
                if let Err(e) = fs::write(&output_path, cobol_code) {
                    error!("Error writing to file '{}': {}", output_path, e);
                    process::exit(1);
                }
            } else {
                println!("{}", cobol_code);
            }
        }
        Err(err) => {
            error!("Compilation error: {}", err);
            process::exit(1);
        }
    }
}
