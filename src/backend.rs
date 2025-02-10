//! # Backend Module
//!
//! This module translates the internal intermediate representation (IR)
//! into COBOL source code. It processes each IR function (currently, only the
//! `main` function is fully supported) and converts IR statements into their
//! equivalent COBOL constructs.
//!
//! Supported IR statement kinds include:
//! - `Move`: Translated into a COBOL MOVE statement.
//! - `Display`: Translated into a COBOL DISPLAY statement.
//! - `If`: Translated into COBOL IF/ELSE/END-IF blocks.
//! - `While`: Translated into a PERFORM UNTIL loop.
//! - `For`: Translated into a PERFORM VARYING loop.
//! - `Unhandled`: Emitted as a comment.
//!
//! The complete COBOL program is assembled with a standard header and footer.

use crate::ir::{IrProgram, IrStmt};
use crate::transpile::CompileError;

/// Generates COBOL code from the given IR program.
///
/// # Arguments
///
/// * `ir` - A reference to the IR program containing functions and statements.
///
/// # Returns
///
/// A `Result` containing the complete COBOL program as a `String` if successful,
/// or a `CompileError` if an error occurs during code generation.
pub fn generate_cobol(ir: &IrProgram) -> Result<String, CompileError> {
    // Assemble the standard COBOL header.
    let mut lines = vec![
        "IDENTIFICATION DIVISION.".to_string(),
        "PROGRAM-ID. GENERATED.".to_string(),
        "AUTHOR. Auto-Generated.".to_string(),
        "DATE-WRITTEN. 2025.".to_string(),
        "ENVIRONMENT DIVISION.".to_string(),
        "INPUT-OUTPUT SECTION.".to_string(),
        "FILE-CONTROL.".to_string(),
        "DATA DIVISION.".to_string(),
        "WORKING-STORAGE SECTION.".to_string(),
        "PROCEDURE DIVISION.".to_string(),
    ];

    // Process each function in the IR.
    // Currently, we fully transpile the `main` function and emit a comment for others.
    for func in &ir.functions {
        if func.name == "main" {
            lines.push(format!("{}-PARA.", func.name.to_uppercase()));
            for stmt in &func.body {
                lines.extend(generate_stmt(stmt));
            }
        } else {
            // Other functions are not yet supported for COBOL code generation.
            lines.push(format!("* Function {} is not supported.", func.name));
        }
    }

    // Append the COBOL footer.
    lines.push("GOBACK.".to_string());
    lines.push("STOP RUN.".to_string());

    Ok(lines.join("\n"))
}

/// Generates COBOL code for a single IR statement.
///
/// # Arguments
///
/// * `stmt` - A reference to an IR statement.
///
/// # Returns
///
/// A vector of strings, each representing a line of COBOL code.
fn generate_stmt(stmt: &IrStmt) -> Vec<String> {
    match stmt {
        IrStmt::Move { var, value } => {
            vec![format!("MOVE {} TO {}.", value, var)]
        }
        IrStmt::Display(text) => {
            vec![format!("DISPLAY \"{}\".", text)]
        }
        IrStmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut lines = vec![format!("IF {} THEN", condition)];
            for s in then_branch {
                // Indent the then-branch lines.
                lines.extend(generate_stmt(s).into_iter().map(|l| format!("  {}", l)));
            }
            if let Some(else_branch) = else_branch {
                lines.push("ELSE".to_string());
                for s in else_branch {
                    // Indent the else-branch lines.
                    lines.extend(generate_stmt(s).into_iter().map(|l| format!("  {}", l)));
                }
            }
            lines.push("END-IF.".to_string());
            lines
        }
        IrStmt::While { condition, body } => {
            let mut lines = vec![format!("PERFORM UNTIL NOT ({})", condition)];
            for s in body {
                lines.extend(generate_stmt(s).into_iter().map(|l| format!("  {}", l)));
            }
            lines.push("END-PERFORM.".to_string());
            lines
        }
        IrStmt::For { var, upper_bound, body } => {
            let mut lines = vec![format!(
                "PERFORM VARYING {} FROM 1 BY 1 UNTIL {} > {}",
                var, var, upper_bound
            )];
            for s in body {
                lines.extend(generate_stmt(s).into_iter().map(|l| format!("  {}", l)));
            }
            lines.push("END-PERFORM.".to_string());
            lines
        }
        IrStmt::Unhandled(text) => {
            vec![format!("* [Unhandled IR: {}]", text)]
        }
    }
}
