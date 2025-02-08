use crate::ast::{Ast, Statement, Expression, DataType, StructField};

pub struct SemanticAnalyzer;

impl SemanticAnalyzer {
    pub fn analyze(ast: &Ast) -> Result<(), String> {
        for stmt in &ast.statements {
            Self::analyze_statement(stmt)?;
        }
        Ok(())
    }

    fn analyze_statement(stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Function { name, parameters, body } => {
                if name.is_empty() {
                    return Err("Function name cannot be empty".to_string());
                }
                for (param_name, _) in parameters {
                    if param_name.is_empty() {
                        return Err("Parameter name cannot be empty".to_string());
                    }
                }
                for stmt in body {
                    Self::analyze_statement(stmt)?;
                }
            }
            Statement::Let { name, value } => {
                if name.is_empty() {
                    return Err("Variable name cannot be empty".to_string());
                }
                Self::analyze_expression(&value)?;
            }
            Statement::Return { value } => {
                Self::analyze_expression(&value)?;
            }
            Statement::Expression(expr) => {
                Self::analyze_expression(expr)?;
            }
            Statement::Struct { name, fields } => {
                if name.is_empty() {
                    return Err("Struct name cannot be empty".to_string());
                }
                for field in fields {
                    if field.name.is_empty() {
                        return Err("Field name cannot be empty".to_string());
                    }
                }
            }
            Statement::Enum { name, variants } => {
                if name.is_empty() {
                    return Err("Enum name cannot be empty".to_string());
                }
                for variant in variants {
                    if variant.is_empty() {
                        return Err("Variant name cannot be empty".to_string());
                    }
                }
            }
            Statement::Module { name, statements } => {
                if name.is_empty() {
                    return Err("Module name cannot be empty".to_string());
                }
                for stmt in statements {
                    Self::analyze_statement(stmt)?;
                }
            }
            Statement::Use { path } => {
                if path.is_empty() {
                    return Err("Use path cannot be empty".to_string());
                }
            }
            Statement::Break => {}
            Statement::Continue => {}
            _ => {}
        }
        Ok(())
    }

    fn analyze_expression(expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::If { condition, consequence, alternative } => {
                Self::analyze_expression(condition)?;
                for stmt in consequence {
                    Self::analyze_statement(stmt)?;
                }
                if let Some(alt) = alternative {
                    for stmt in alt {
                        Self::analyze_statement(stmt)?;
                    }
                }
            }
            Expression::While { condition, body } => {
                Self::analyze_expression(condition)?;
                for stmt in body {
                    Self::analyze_statement(stmt)?;
                }
            }
            Expression::For { identifier, iterable, body } => {
                if identifier.is_empty() {
                    return Err("Identifier cannot be empty".to_string());
                }
                Self::analyze_expression(iterable)?;
                for stmt in body {
                    Self::analyze_statement(stmt)?;
                }
            }
            Expression::Match { value, cases } => {
                Self::analyze_expression(value)?;
                for (pattern, consequence) in cases {
                    Self::analyze_expression(pattern)?;
                    for stmt in consequence {
                        Self::analyze_statement(stmt)?;
                    }
                }
            }
            Expression::Binary { left, operator, right } => {
                Self::analyze_expression(left)?;
                Self::analyze_expression(right)?;
            }
            Expression::Unary { operator, right } => {
                Self::analyze_expression(right)?;
            }
            Expression::Call { function, arguments } => {
                Self::analyze_expression(function)?;
                for arg in arguments {
                    Self::analyze_expression(arg)?;
                }
            }
            Expression::Array(elements) | Expression::Tuple(elements) => {
                for elem in elements {
                    Self::analyze_expression(elem)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}
