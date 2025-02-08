#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Existing tokens...
    Array,
    Tuple,
    Break,
    Continue,
    As,
    Ref,
    Mut,
    Lifetime,
    Dot,
    Comma,
    LSquare,
    RSquare,
    At,
    In,
    Bang,
    DoubleColon,
    QuestionMark,
    ColonColon,
    FatArrow,
    ThinArrow,
    Pipe,
    Ampersand,
    Tilde,
    Caret,
    Backslash,
    Hash,
    Dollar,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    StringLiteral,
    Identifier,
    If,
    Else,
    While,
    For,
    Match,
    Return,
    EOF,
}

impl<'a> Lexer<'a> {
    // Existing methods...

    pub fn new(input: &'a str) -> Self {
        self.input = input;
        self.position = 0;
        self.read_position = 0;
        self.ch = None;
        self.read_char();
        self.read_char();
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
            self.position = self.read_position + 1;
        } else {
            self.ch = self.input[self.read_position..].chars().next();
            self.read_position += 1;
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input[self.read_position..].chars().next()
        }
    }

    pub fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            Some(ch) if ch.is_alphabetic() => {
                let ident = self.read_identifier();
                return match ident.as_str() {
                    "fn" => Token::Fn,
                    "let" => Token::Let,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "while" => Token::While,
                    "for" => Token::For,
                    "match" => Token::Match,
                    "return" => Token::Return,
                    "true" => Token::True,
                    "false" => Token::False,
                    "struct" => Token::Struct,
                    "enum" => Token::Enum,
                    "mod" => Token::Mod,
                    "use" => Token::Use,
                    "as" => Token::As,
                    "ref" => Token::Ref,
                    "mut" => Token::Mut,
                    "lifetime" => Token::Lifetime,
                    "dot" => Token::Dot,
                    "comma" => Token::Comma,
                    "lsquare" => Token::LSquare,
                    "rsquare" => Token::RSquare,
                    "at" => Token::At,
                    "in" => Token::In,
                    "bang" => Token::Bang,
                    "doublecolon" => Token::DoubleColon,
                    "questionmark" => Token::QuestionMark,
                    "coloncolon" => Token::ColonColon,
                    "fatarrow" => Token::FatArrow,
                    "thinarrow" => Token::ThinArrow,
                    "pipe" => Token::Pipe,
                    "ampersand" => Token::Ampersand,
                    "tilde" => Token::Tilde,
                    "caret" => Token::Caret,
                    "backslash" => Token::Backslash,
                    "hash" => Token::Hash,
                    "dollar" => Token::Dollar,
                    "equals" => Token::Equals,
                    "notequals" => Token::NotEquals,
                    "lessthan" => Token::LessThan,
                    "greaterthan" => Token::GreaterThan,
                    "plus" => Token::Plus,
                    "minus" => Token::Minus,
                    "asterisk" => Token::Asterisk,
                    "slash" => Token::Slash,
                    "colon" => Token::Colon,
                    "semicolon" => Token::Semicolon,
                    "lparen" => Token::LParen,
                    "rparen" => Token::RParen,
                    "lbrace" => Token::LBrace,
                    "rbrace" => Token::RBrace,
                    "stringliteral" => Token::StringLiteral,
                    "identifier" => Token::Identifier,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "while" => Token::While,
                    "for" => Token::For,
                    "match" => Token::Match,
                    "return" => Token::Return,
                    "eof" => Token::EOF,
                    _ => Token::Illegal,
                };

        self.read_char();
        tok
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
