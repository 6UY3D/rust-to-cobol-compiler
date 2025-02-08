/// The root node of the AST representing an entire program.
#[derive(Debug, Clone)]
pub struct Program {
    /// A list of statements in the program.
    pub statements: Vec<Statement>,
}

/// A statement in the program.
#[derive(Debug, Clone)]
pub enum Statement {
    /// A variable declaration, e.g., `let x = 5;`
    Let {
        /// The identifier being declared.
        identifier: String,
        /// The expression assigned to the identifier.
        value: Expression,
    },
    /// An expression statement, which evaluates an expression.
    Expression(Expression),
    /// A return statement, e.g., `return x;`
    Return(Expression),
    // You can add more statement variants as needed.
}

/// An expression in the program.
#[derive(Debug, Clone)]
pub enum Expression {
    /// A literal value such as a number, boolean, or string.
    Literal(Literal),
    /// A reference to a variable, e.g., `x`
    Identifier(String),
    /// A binary operation, e.g., `a + b`
    BinaryOp {
        /// The left-hand side expression.
        left: Box<Expression>,
        /// The operator.
        operator: BinaryOperator,
        /// The right-hand side expression.
        right: Box<Expression>,
    },
    /// A function call expression, e.g., `foo(1, 2)`
    FunctionCall {
        /// The function being called.
        callee: Box<Expression>,
        /// The list of arguments passed to the function.
        arguments: Vec<Expression>,
    },
    // Extend with additional expression types as necessary.
}

/// A literal value.
#[derive(Debug, Clone)]
pub enum Literal {
    /// An integer literal.
    Integer(i64),
    /// A floating-point literal.
    Float(f64),
    /// A boolean literal.
    Boolean(bool),
    /// A string literal.
    String(String),
}

/// Binary operators available in expressions.
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,       // +
    Subtract,  // -
    Multiply,  // *
    Divide,    // /
    // Extend with additional operators if needed.
}
