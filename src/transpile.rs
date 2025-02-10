use crate::backend::generate_cobol;
use crate::frontend_rustc::parse_rust_source;
use crate::mir_lowering::lower_mir;
use crate::monomorphizer::monomorphize;
use crate::trait_resolution::resolve_traits;
use crate::closure_transform::transform_closures;
use crate::async_transform::transform_async;
use crate::macro_expansion::expand_macros;
use crate::lifetime_erasure::erase_lifetimes;
use crate::ir::IrProgram;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Frontend error: {0}")]
    FrontendError(String),
    #[error("Lowering error: {0}")]
    LoweringError(String),
    #[error("Backend error: {0}")]
    BackendError(String),
    #[error("Other error: {0}")]
    Other(String),
}

/// The full compilation pipeline using rustc’s parser and MIR.
/// This function processes the entire Rust source and outputs COBOL code.
pub fn compile_full_rustc(source: &str, dump_ast: bool) -> Result<String, CompileError> {
    // 1. Use rustc's parser to obtain a full AST/HIR dump.
    let ast_dump = parse_rust_source(source)
        .map_err(|e| CompileError::FrontendError(e.to_string()))?;
    
    if dump_ast {
        println!("--- AST Dump (from rustc) ---");
        println!("{}", ast_dump);
        println!("--- End AST Dump ---");
    }
    
    // 2. Lower the rustc AST/HIR into our custom IR.
    let mut ir_program: IrProgram = lower_mir(&ast_dump)
        .map_err(|e| CompileError::LoweringError(e))?;
    
    // 3. Run transformation passes.
    ir_program = monomorphize(ir_program);
    ir_program = resolve_traits(ir_program);
    ir_program = transform_closures(ir_program);
    ir_program = transform_async(ir_program);
    ir_program = expand_macros(ir_program);
    ir_program = erase_lifetimes(ir_program);
    
    // 4. Generate COBOL code from the IR.
    let cobol_code = generate_cobol(&ir_program)
        .map_err(|e| CompileError::BackendError(e.to_string()))?;
    
    Ok(cobol_code)
}
