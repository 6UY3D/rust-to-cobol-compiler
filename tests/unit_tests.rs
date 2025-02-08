#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic_analyzer::SemanticAnalyzer;
    use crate::code_generator::CodeGenerator;
    use crate::lexer::Token;

    #[test]
    fn test_lexer() {
        let input = "fn main(x: i32) { let y = 42; if x > 0 { return y; } else { return 0; } } struct Point { x: i32, y: i32 } enum Color { Red, Green, Blue } mod geometry { use std::f64; } let arr = [1, 2, 3]; let tup = (1, 2); for i in arr { println!(\"{}\", i); }";
        let mut lexer = Lexer::new(input);

        let tokens = vec![
            Token::Fn,
            Token::Ident("main".to_string()),
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Colon,
            Token::Ident("i32".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Let,
            Token::Ident("y".to_string()),
            Token::Assign,
            Token::Int("42".to_string()),
            Token::If,
            Token::Ident("x".to_string()),
            Token::GT,
            Token::Int("0".to_string()),
            Token::LBrace,
            Token::Return,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::Int("0".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::RBrace,
            Token::Struct,
            Token::Ident("Point".to_string()),
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Colon,
            Token::Ident("i32".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Colon,
            Token::Ident("i32".to_string()),
            Token::RBrace,
            Token::Enum,
            Token::Ident("Color".to_string()),
            Token::LBrace,
            Token::Ident("Red".to_string()),
            Token::Comma,
            Token::Ident("Green".to_string()),
            Token::Comma,
            Token::Ident("Blue".to_string()),
            Token::RBrace,
            Token::Mod,
            Token::Ident("geometry".to_string()),
            Token::LBrace,
            Token::Use,
            Token::Ident("std".to_string()),
            Token::DoubleColon,
            Token::Ident("f64".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Let,
            Token::Ident("arr".to_string()),
            Token::Assign,
            Token::LSquare,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Comma,
            Token::Int("3".to_string()),
            Token::RSquare,
            Token::Let,
            Token::Ident("tup".to_string()),
            Token::Assign,
            Token::LParen,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::RParen,
            Token::For,
            Token::Ident("i".to_string()),
            Token::Ident("arr".to_string()),
            Token::LBrace,
            Token::Ident("println".to_string()),
            Token::Bang,
            Token::LParen,
            Token::StringLiteral("{}".to_string()),
            Token::Comma,
            Token::Ident("i".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::RBrace,
            Token::EOF,
        ];

        for token in tokens {
            assert_eq!(lexer.next_token(), token);
        }
    }

    #[test]
    fn test_parser() {
        let input = "fn main(x: i32) { let y = 42; if x > 0 { return y; } else { return 0; } } struct Point { x: i32, y: i32 } enum Color { Red, Green, Blue } mod geometry { use std::f64; } let arr = [1, 2, 3]; let tup = (1, 2); for i in arr { println!(\"{}\", i); }";
        let mut parser = Parser::new(Lexer::new(input));

        let ast = parser.parse();
        assert_eq!(ast.statements.len(), 7);
    }

    #[test]
    fn test_semantic_analyzer() {
        let input = "fn main(x: i32) { let y = 42; if x > 0 { return y; } else { return 0; } } struct Point { x: i32, y: i32 } enum Color { Red, Green, Blue } mod geometry { use std::f64; } let arr = [1, 2, 3]; let tup = (1, 2); for i in arr { println!(\"{}\", i); }";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast = parser.parse();
        assert!(SemanticAnalyzer::analyze(&ast).is_ok());
    }

    #[test]
    fn test_code_generator() {
        let input = "fn main(x: i32) { let y = 42; if x > 0 { return y; } else { return 0; } } struct Point { x: i32, y: i32 } enum Color { Red, Green, Blue } mod geometry { use std::f64; } let arr = [1, 2, 3]; let tup = (1, 2); for i in arr { println!(\"{}\", i); }";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast = parser.parse();
        let cobol_code = CodeGenerator::generate(&ast);

        assert!(cobol_code.contains("IDENTIFICATION DIVISION."));
        assert!(cobol_code.contains("PROCEDURE DIVISION."));
        assert!(cobol_code.contains("COMPUTE Y = 42."));
        assert!(cobol_code.contains("IF X > 0 THEN"));
        assert!(cobol_code.contains("MOVE Y TO RETURN-VALUE."));
        assert!(cobol_code.contains("POINT-STRUCT."));
        assert!(cobol_code.contains("COLOR-ENUM."));
        assert!(cobol_code.contains("GEOMETRY-MODULE."));
        assert!(cobol_code.contains("USE STD.F64."));
        assert!(cobol_code.contains("MOVE (1, 2, 3) TO ARRAY-OR-TUPLE."));
        assert!(cobol_code.contains("PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARR"));
    }
}
