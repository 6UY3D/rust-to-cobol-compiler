# Rust-to-COBOL Compiler

A production‑ready compiler that transpiles Rust source code into COBOL. This project leverages the Rust compiler’s own parser and semantic analysis (via unstable internal APIs such as `rustc_interface` and `rustc_driver`) to support a large subset of the Rust language—including generics, traits, closures, async functions, macros, lifetimes, and more—and generates equivalent COBOL code.

> **Warning:**  
> This project uses unstable internal Rust APIs and must be compiled on a nightly toolchain. To build successfully, you may need to set `RUSTC_BOOTSTRAP=1` or patch your Cargo configuration to access the `rustc_private` crates.

## Overview

Rust-to-COBOL Compiler is designed to convert Rust programs into COBOL code. The project is organized into several major components:

- **Frontend:**  
  Uses the Rust compiler’s own parser (via `rustc_interface`/`rustc_driver`) to obtain the complete Abstract Syntax Tree (AST), HIR, and MIR for full language support. Macro expansion, type checking, and trait resolution are performed as part of Rust’s built‑in analysis.

- **Intermediate Representation (IR):**  
  The compiler lowers the rich Rust semantic information into a simplified, custom IR that represents functions, variables, control flow, expressions, and more.

- **Transformation Passes:**  
  The IR is then transformed through several phases:
  - **Monomorphization:** Specializes generic functions.
  - **Trait Resolution:** Replaces trait method calls with concrete implementations.
  - **Closure & Async Transform:** Converts closures and async functions into equivalent state-machine representations.
  - **Macro Expansion & Lifetime Erasure:** Fully expands macros and removes lifetime annotations that have no COBOL equivalent.

- **Backend:**  
  The transformed IR is mapped into COBOL code. Functions are converted into COBOL paragraphs, let‑bindings into MOVE/COMPUTE statements, control flow into COBOL IF/ELSE and PERFORM loops, and any unhandled constructs are emitted as comments.

- **CLI:**  
  A full‑featured command‑line interface (built with [clap](https://crates.io/crates/clap) and [env_logger](https://crates.io/crates/env_logger)) supports reading input from a file or STDIN, specifying an output file, and enabling verbose logging and AST dumps for debugging.

## Features

- **Full Language Support:**  
  Designed to eventually support the entire Rust language—including generics, traits, closures, async, macros, and lifetimes—by integrating with rustc’s internal interfaces.

- **Robust Error Handling:**  
  Uses `thiserror` for clear and comprehensive error reporting throughout the compilation pipeline.

- **Extensible Architecture:**  
  The compiler is modularized into frontend, IR lowering, transformation passes, and backend generation. This structure makes it easy to extend or modify individual phases.

- **Production-Ready CLI:**  
  The command‑line interface accepts multiple options:
  - Input source (file or STDIN)
  - Optional output file
  - Verbosity control
  - Option to dump the full AST (from rustc)

## Installation

To build this project, you must use a nightly Rust toolchain because of the unstable rustc APIs used:

1. **Install Nightly Rust:**

   ```bash
   rustup install nightly
   rustup default nightly
   ```

2. **Clone the Repository:**
   ```bash
   git clone https://github.com/6UY3D/rust-to-cobol-compiler.git
   cd rust-to-cobol-compiler
   ```

3. **Build the Project:**
   ```bash
   cargo build --release
   ```

Tip: You might need to set the environment variable to allow unstable features:
   ```bash
   export RUSTC_BOOTSTRAP=1
   ```

## Usage
After building, run the compiler from the command line. For example:
   ```bash
   # Compile a Rust source file into COBOL and print the output to stdout:
   ./target/release/rust-to-cobol-compiler path/to/source.rs

   # Read Rust source from STDIN and write COBOL output to a file:
   cat path/to/source.rs | ./target/release/rust-to-cobol-compiler -o output.cob

   # Increase verbosity and dump the full AST from rustc:
   ./target/release/rust-to-cobol-compiler path/to/source.rs --dump-ast -v
   ```

The CLI options include:

- <input>: Path to the Rust source file. Use - to read from STDIN.
- `-o, --output <OUTPUT>`: Optional output file path.
- `-v, --verbose`: Increase logging verbosity.
- `--dump-ast`: Dump the complete AST (obtained via rustc’s parser) to stdout for debugging.

## Architecture
The compiler is divided into the following modules:

- ``frontend_rustc.rs``: Integrates with rustc to obtain full parsing and semantic analysis.
- ``mir_lowering.rs``: Converts rustc’s HIR/MIR into a simplified intermediate representation (IR).
- **Transformation Passes:**
Modules such as ``monomorphizer.rs``, ``trait_resolution.rs``, ``closure_transform.rs``, ``async_transform.rs``, ``macro_expansion.rs``, and ``lifetime_erasure.rs`` process the IR to handle full Rust semantics.
- ``backend.rs``: Generates COBOL code from the final IR.
- ``transpile.rs``: Orchestrates the full pipeline from parsing to code generation.




