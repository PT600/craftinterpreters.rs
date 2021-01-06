use anyhow::{Result, bail};
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    QUESTION,

    // One or two character tokens.
    BANG,
    BangEqual,
    EQUAL,
    EqualEqual,
    GREATER,
    GreaterEqual,
    LESS,
    LessEqual,

    // Literals.
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),

    KEYWORD(String),
    // Keywords.
    AND,
    CLASS,
    ELSE,
    False,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    True,
    VAR,
    WHILE,

    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(LiteralKind),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expr>),
    Ternary(Box<TernaryExpr>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Boolean(bool),
    Nil,
    Identifier(String),
    String(String),
    Num(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub left: Expr,
    pub right: Expr,
}

pub struct Object {}

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Num(f64),
    Nil,
    String(String),
    // Object(Object),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn as_num(&self) -> Result<f64> {
        match self {
            Value::Num(num) => Ok(*num),
            _ => bail!("Operands must be a number"),
        }
    }

    pub fn is_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, _) | (_, Value::Nil) => false,
            _ => self == other,
        }
    }
}

pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
}