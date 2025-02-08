use crate::ast::{Ast, Statement, Expression, StructField};

pub struct CodeGenerator;

impl CodeGenerator {
    pub fn generate(ast: &Ast) -> String {
        let mut cobol_code = String::new();

        cobol_code.push_str(&Self::generate_identification_division());
        cobol_code.push_str(&Self::generate_environment_division());
        cobol_code.push_str(&Self::generate_data_division());
        cobol_code.push_str(&Self::generate_procedure_division(ast));

        cobol_code
    }

    fn generate_identification_division() -> String {
        "IDENTIFICATION DIVISION.\nPROGRAM-ID. HelloWorld.\n".to_string()
    }

    fn generate_environment_division() -> String {
        "ENVIRONMENT DIVISION.\n".to_string()
    }

    fn generate_data_division() -> String {
        "DATA DIVISION.\nWORKING-STORAGE SECTION.\n".to_string()
    }

    fn generate_procedure_division(ast: &Ast) -> String {
        let mut procedure_division = String::new();

        for stmt in &ast.statements {
            procedure_division.push_str(&Self::generate_statement(stmt));
        }

        procedure_division.push_str("    STOP RUN.\n");
        procedure_division
    }

    fn generate_statement(stmt: &Statement) -> String {
        match stmt {
            Statement::Function { name, parameters: _, body } => {
                let mut func_code = format!("{}-PROC.\n", name.to_uppercase());
                for stmt in body {
                    func_code.push_str(&Self::generate_statement(stmt));
                }
                func_code
            }
            Statement::Let { name, value } => {
                format!("    COMPUTE {} = {}.\n", name.to_uppercase(), Self::generate_expression(&value))
            }
            Statement::Return { value } => {
                format!("    MOVE {} TO RETURN-VALUE.\n", Self::generate_expression(&value))
            }
            Statement::Expression(expr) => {
                Self::generate_expression_statement(expr)
            }
            Statement::Struct { name, fields } => {
                let mut struct_code = format!("01 {}-STRUCT.\n", name.to_uppercase());
                for field in fields {
                    struct_code.push_str(&format!("    05 {} PIC X.\n", field.name.to_uppercase()));
                }
                struct_code
            }
            Statement::Enum { name, variants } => {
                let mut enum_code = format!("01 {}-ENUM.\n", name.to_uppercase());
                for variant in variants {
                    enum_code.push_str(&format!("    05 {} PIC X.\n", variant.to_uppercase()));
                }
                enum_code
            }
            Statement::Module { name, statements } => {
                let mut module_code = format!("{}-MODULE.\n", name.to_uppercase());
                for stmt in statements {
                    module_code.push_str(&Self::generate_statement(stmt));
                }
                module_code
            }
            Statement::Use { path } => {
                format!("    USE {}.\n", path.join(".").to_uppercase())
            }
            Statement::Break => {
                "    EXIT.\n".to_string()
            }
            Statement::Continue => {
                "    CONTINUE.\n".to_string()
            }
            _ => String::new(),
        }
    }

    fn generate_expression_statement(expr: &Expression) -> String {
        match expr {
            Expression::If { condition, consequence, alternative } => {
                let mut if_code = format!("    IF {} THEN\n", Self::generate_expression(condition));
                for stmt in consequence {
                    if_code.push_str(&Self::generate_statement(stmt));
                }
                if let Some(alt) = alternative {
                    if_code.push_str("    ELSE\n");
                    for stmt in alt {
                        if_code.push_str(&Self::generate_statement(stmt));
                    }
                }
                if_code.push_str("    END-IF.\n");
                if_code
            }
            Expression::While { condition, body } => {
                let mut while_code = format!("    PERFORM UNTIL {} = FALSE\n", Self::generate_expression(condition));
                for stmt in body {
                    while_code.push_str(&Self::generate_statement(stmt));
                }
                while_code.push_str("    END-PERFORM.\n");
                while_code
            }
            Expression::For { identifier, iterable, body } => {
                let mut for_code = format!("    PERFORM VARYING {} FROM 1 BY 1 UNTIL {} > {}\n", identifier.to_uppercase(), identifier.to_uppercase(), Self::generate_expression(iterable));
                for stmt in body {
                    for_code.push_str(&Self::generate_statement(stmt));
                }
                for_code.push_str("    END-PERFORM.\n");
                for_code
            }
            Expression::Match { value, cases } => {
                let mut match_code = format!("    EVALUATE {}.\n", Self::generate_expression(value));
                for (pattern, consequence) in cases {
                    match_code.push_str(&format!("        WHEN {} \n", Self::generate_expression(pattern)));
                    for stmt in consequence {
                        match_code.push_str(&Self::generate_statement(stmt));
                    }
                }
                match_code.push_str("    END-EVALUATE.\n");
                match_code
            }
            Expression::Binary { left, operator, right } => {
                format!("    COMPUTE {} = {} {}.\n", Self::generate_expression(left), operator, Self::generate_expression(right))
            }
            Expression::Unary { operator, right } => {
                format!("    COMPUTE {} = {}{}.\n", operator, Self::generate_expression(right))
            }
            Expression::Call { function, arguments } => {
                let mut call_code = format!("    CALL '{}' USING {}.\n", Self::generate_expression(function), arguments.iter().map(|arg| Self::generate_expression(arg)).collect::<Vec<_>>().join(", "));
                call_code
            }
            Expression::Array(elements) | Expression::Tuple(elements) => {
                let mut elements_code = format!("    MOVE ({}) TO ARRAY-OR-TUPLE.\n", elements.iter().map(|elem| Self::generate_expression(elem)).collect::<Vec<_>>().join(", "));
                elements_code
            }
            _ => String::new(),
        }
    }

    fn generate_expression(expr: &Expression) -> String {
        match expr {
            Expression::Integer(value) => value.clone(),
            Expression::Float(value) => value.clone(),
            Expression::Boolean(value) => if value { "TRUE".to_string() } else { "FALSE".to_string() },
            Expression::Identifier(ident) => ident.to_uppercase(),
            Expression::StringLiteral(value) => format!("\"{}\"", value),
            _ => String::new(),
        }
    }
}
