#![feature(rustc_private)]

use clap::Parser;
use log::{debug, error, info};
use std::{
    fs,
    io::{self, Read},
    process,
};

use rust_to_cobol_compiler::{compile_full_rustc, CompileError};

/// A full-featured Rust-to-COBOL compiler.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to the input Rust source file (use "-" to read from STDIN).
    #[arg(default_value = "-")]
    input: String,

    /// Optional path to the output COBOL file; if omitted, output is printed to stdout.
    #[arg(short, long)]
    output: Option<String>,

    /// Increase verbosity (e.g., -v, -vv).
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Dump the full AST (from rustc) to stdout.
    #[arg(long)]
    dump_ast: bool,
}

fn main() {
    env_logger::init();
    let cli = Cli::parse();

    let source = if cli.input == "-" {
        info!("Reading source from STDIN");
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer).unwrap_or_else(|e| {
            error!("Error reading STDIN: {}", e);
            process::exit(1);
        });
        buffer
    } else {
        info!("Reading source from file: {}", cli.input);
        fs::read_to_string(&cli.input).unwrap_or_else(|e| {
            error!("Error reading file '{}': {}", cli.input, e);
            process::exit(1);
        })
    };

    debug!("Source length: {} characters", source.len());

    match compile_full_rustc(&source, cli.dump_ast) {
        Ok(cobol_code) => {
            if let Some(output_path) = cli.output {
                fs::write(&output_path, cobol_code).unwrap_or_else(|e| {
                    error!("Error writing to file '{}': {}", output_path, e);
                    process::exit(1);
                });
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

}
