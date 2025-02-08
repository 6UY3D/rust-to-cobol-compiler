use crate::lexer::Token;
use crate::ast::{Ast, Statement, Expression, DataType, StructField};

pub struct Parser<'a> {
    lexer: crate::lexer::Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    // Existing methods...

    pub fn new(lexer: Lexer<'a>) -> Self {
        self.lexer = lexer;
        self.current_token = self.lexer.next_token();
        self.peek_token = self.lexer.next_token();
        self.next_token();
        self.next_token();
    }

    pub fn parse_array(&mut self) -> Expression {
        self.next_token(); // Skip '['
        let elements = self.parse_expression_list();
        self.expect_peek(Token::RSquare);

        Expression::Array(elements)
    }

    pub fn parse_tuple(&mut self) -> Expression {
        self.next_token(); // Skip '('
        let elements = self.parse_expression_list();
        self.expect_peek(Token::RParen);

        Expression::Tuple(elements)
    }

    pub fn parse_expression_list(&mut self) -> Vec<Expression> {
        let mut elements = Vec::new();
        if self.current_token != Token::RSquare && self.current_token != Token::RParen {
            elements.push(self.parse_expression());
            while self.current_token == Token::Comma {
                self.next_token(); // Skip ','
                elements.push(self.parse_expression());
            }
        }
        elements
    }

    pub fn parse_for_expression(&mut self) -> Expression {
        self.next_token(); // Skip 'for'
        let identifier = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Expression::NoOp;
        };
        self.next_token(); // Skip identifier
        self.next_token(); // Skip 'in'

        let iterable = self.parse_expression();
        self.expect_peek(Token::LBrace);

        let body = self.parse_statements();
        self.expect_peek(Token::RBrace);

        Expression::For {
            identifier,
            iterable: Box::new(iterable),
            body,
        }
    }

    pub fn parse_match_expression(&mut self) -> Expression {
        self.next_token(); // Skip 'match'
        let value = self.parse_expression();
        self.expect_peek(Token::LBrace);

        let cases = self.parse_match_cases();

        self.expect_peek(Token::RBrace);

        Expression::Match {
            value: Box::new(value),
            cases,
        }
    }

    pub fn parse_match_cases(&mut self) -> Vec<(Expression, Vec<Statement>)> {
        let mut cases = Vec::new();
        while self.current_token != Token::RBrace {
            let pattern = self.parse_expression();
            self.expect_peek(Token::FatArrow);

            let consequence = self.parse_statements();
            cases.push((pattern, consequence));
        }
        cases
    }

    pub fn parse_function_call(&mut self) -> Expression {
        let function = self.parse_identifier_expression();
        self.expect_peek(Token::LParen);

        let arguments = self.parse_expression_list();
        self.expect_peek(Token::RParen);

        Expression::Call {
            function: Box::new(function),
            arguments,
        }
    }

    pub fn parse_identifier_expression(&mut self) -> Expression {
        let ident = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Expression::NoOp;
        };
        self.next_token(); // Skip identifier

        Expression::Identifier(ident)
    }

    pub fn parse_binary_expression(&mut self, precedence: u8) -> Expression {
        let mut left_expr = self.parse_expression();

        while precedence < self.current_precedence() {
            let operator = self.current_token.clone();
            self.next_token();
            let right_expr = self.parse_expression();
            left_expr = Expression::Binary {
                left: Box::new(left_expr),
                operator: operator.to_string(),
                right: Box::new(right_expr),
            };
        }

        left_expr
    }

    pub fn current_precedence(&self) -> u8 {
        match self.current_token {
            Token::Plus | Token::Minus => 1,
            Token::Asterisk | Token::Slash => 2,
            Token::Caret => 3,
            Token::Ampersand | Token::Pipe => 4,
            Token::LT | Token::GT | Token::EQ | Token::NotEq => 5,
            _ => 0,
        }
    }

    pub fn parse_expression(&mut self) -> Expression {
        match self.current_token {
            Token::Int(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::Integer(value)
            }
            Token::Float(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::Float(value)
            }
            Token::Ident(ref ident) => {
                let ident = ident.clone();
                self.next_token();
                Expression::Identifier(ident)
            }
            Token::True => {
                self.next_token();
                Expression::Boolean(true)
            }
            Token::False => {
                self.next_token();
                Expression::Boolean(false)
            }
            Token::LParen => self.parse_tuple(),
            Token::LSquare => self.parse_array(),
            Token::If => self.parse_if_expression(),
            Token::Else => self.parse_else_expression(),
            Token::While => self.parse_while_expression(),
            Token::For => self.parse_for_expression(),
            Token::Match => self.parse_match_expression(),
            Token::Return => self.parse_return_statement(),
            Token::StringLiteral(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::StringLiteral(value)
            }
            Token::Bang => {
                self.next_token();
                let expr = self.parse_expression();
                Expression::Unary {
                    operator: "!".to_string(),
                    right: Box::new(expr),
                }
            }
            Token::Minus => {
                self.next_token();
                let expr = self.parse_expression();
                Expression::Unary {
                    operator: "-".to_string(),
                    right: Box::new(expr),
                }
            }
            _ => Expression::NoOp,
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        match self.current_token {
            Token::Fn => self.parse_function(),
            Token::Let => self.parse_let_statement(),
            Token::If => self.parse_if_statement(),
            Token::Else => self.parse_else_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            Token::Match => self.parse_match_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Break => Statement::Break,
            Token::Continue => Statement::Continue,
            Token::Struct => self.parse_struct(),
            Token::Enum => self.parse_enum(),
            Token::Mod => self.parse_module(),
            Token::Use => self.parse_use(),
            _ => Statement::Expression(self.parse_expression()),
        }
    }

    pub fn parse_let_statement(&mut self) -> Statement {
        self.next_token(); // Skip 'let'
        let name = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Statement::NoOp;
        };
        self.next_token(); // Skip variable name
        self.next_token(); // Skip '='

        let value = self.parse_expression();

        Statement::Let { name, value }
    }

    pub fn parse_return_statement(&mut self) -> Statement {
        self.next_token(); // Skip 'return'
        let value = self.parse_expression();

        Statement::Return { value }
    }

    pub fn parse_break_statement(&mut self) -> Statement {
        self.next_token(); // Skip 'break'
        Statement::Break
    }

    pub fn parse_continue_statement(&mut self) -> Statement {
        self.next_token(); // Skip 'continue'
        Statement::Continue
    }

    pub fn parse_struct(&mut self) -> Statement {
        self.next_token(); // Skip 'struct'
        let name = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Statement::NoOp;
        };
        self.next_token(); // Skip struct name
        self.next_token(); // Skip '{'

        let fields = self.parse_struct_fields();

        self.expect_peek(Token::RBrace);

        Statement::Struct { name, fields }
    }

    pub fn parse_struct_fields(&mut self) -> Vec<StructField> {
        let mut fields = Vec::new();
        while self.current_token != Token::RBrace {
            let name = if let Token::Ident(ref ident) = self.current_token {
                ident.clone()
            } else {
                return Vec::new();
            };
            self.next_token(); // Skip field name
            self.next_token(); // Skip ':'

            let data_type = self.parse_data_type();
            fields.push(StructField { name, data_type });

            if self.current_token == Token::Comma {
                self.next_token(); // Skip ','
                fields.push(StructField { name, data_type });
            }
        }
        fields
    }

    pub fn parse_enum(&mut self) -> Statement {
        self.next_token(); // Skip 'enum'
        let name = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Statement::NoOp;
        };
        self.next_token(); // Skip enum name
        self.next_token(); // Skip '{'

        let variants = self.parse_enum_variants();

        self.expect_peek(Token::RBrace);

        Statement::Enum { name, variants }
    }

    pub fn parse_enum_variants(&mut self) -> Vec<String> {
        let mut variants = Vec::new();
        while self.current_token != Token::RBrace {
            let variant = if let Token::Ident(ref ident) = self.current_token {
                ident.clone()
            } else {
                return Vec::new();
            };
            self.next_token(); // Skip variant name
            self.next_token(); // Skip ','
            variants.push(variant);
        }
        variants
    }

    pub fn parse_module(&mut self) -> Statement {
        self.next_token(); // Skip 'mod'
        let name = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return Statement::NoOp;
        };
        self.next_token(); // Skip module name
        self.next_token(); // Skip '{'

        let statements = self.parse_statements();

        self.expect_peek(Token::RBrace);

        Statement::Module { name, statements }
    }

    pub fn parse_use(&mut self) -> Statement {
        self.next_token(); // Skip 'use'
        let path = self.parse_use_path();

        self.expect_peek(Token::Semicolon);

        Statement::Use { path }
    }

    pub fn parse_use_path(&mut self) -> Vec<String> {
        let mut path = Vec::new();
        while self.current_token != Token::Semicolon {
            let segment = if let Token::Ident(ref ident) = self.current_token {
                ident.clone()
            } else {
                return Vec::new();
            };
            self.next_token(); // Skip path segment
            path.push(segment);

            if self.current_token == Token::DoubleColon {
                self.next_token(); // Skip '::'
            } else {
                break;
            }
        }
        path
    }

    pub fn parse_data_type(&mut self) -> DataType {
        let ident = if let Token::Ident(ref ident) = self.current_token {
            ident.clone()
        } else {
            return DataType::Unknown;
        };
        self.next_token(); // Skip data type identifier

        DataType::Custom(ident)
    }

    pub fn expect_peek(&mut self, t: Token) {
        if self.peek_token == t {
            self.next_token();
        } else {
            panic!("expected {:?}, got {:?}", t, self.peek_token);
        }
    }

    pub fn next_token(&mut self) -> Token {
        let tok = self.current_token.clone();
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.next_token();
        tok
    }

    pub fn parse_statements(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.current_token != Token::RBrace {
            statements.push(self.parse_statement());
            if self.current_token == Token::Comma {
                self.next_token(); // Skip ','
                statements.push(self.parse_statement());
            }
        }
        statements
    }

    pub fn parse_expression(&mut self) -> Expression {
        match self.current_token {
            Token::Int(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::Integer(value)
            }
            Token::Float(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::Float(value)
            }
            Token::Ident(ref ident) => {
                let ident = ident.clone();
                self.next_token();
                Expression::Identifier(ident)
            }
            Token::True => {
                self.next_token();
                Expression::Boolean(true)
            }
            Token::False => {
                self.next_token();
                Expression::Boolean(false)
            }
            Token::LParen => self.parse_tuple(),
            Token::LSquare => self.parse_array(),
            Token::If => self.parse_if_expression(),
            Token::Else => self.parse_else_expression(),
            Token::While => self.parse_while_expression(),
            Token::For => self.parse_for_expression(),
            Token::Match => self.parse_match_expression(),
            Token::Return => self.parse_return_statement(),
            Token::StringLiteral(ref value) => {
                let value = value.clone();
                self.next_token();
                Expression::StringLiteral(value)
            }
            Token::Bang => {
                self.next_token();
                let expr = self.parse_expression();
                Expression::Unary {
                    operator: "!".to_string(),
                    right: Box::new(expr),
                }
            }
            Token::Minus => {
                self.next_token();
                let expr = self.parse_expression();
                Expression::Unary {
                    operator: "-".to_string(),
                    right: Box::new(expr),
                }
            }
            _ => Expression::NoOp,
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input[self.read_position..].chars().next()
        }
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        let ident = self.input[position..self.read_position].to_string();
        self.position = self.read_position + ident.len();
        ident
    }

    pub fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        let num = self.input[position..self.read_position].to_string();
        self.position = self.read_position + num.len();
        num
    }

    pub fn read_string(&mut self) -> String {
        let position = self.position;
        let mut result = String::new();
        self.ch = self.input[position..].chars().next();
        while self.ch != '"' && self.ch != '\\' {
            result.push(self.ch);
            self.read_char();
        }
        if self.ch == '"' {
            self.read_char();
            let mut escaped = false;
            while self.ch != '\\' && self.ch != '"' {
                result.push(self.ch);
                self.read_char();
            }
            if self.ch == '\\' {
                escaped = true;
                self.read_char();
            }
            if self.ch == '"' {
                escaped = false;
                self.read_char();
            }
        }
        result
    }
}
