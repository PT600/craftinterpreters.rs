use std::fmt::{Display, write};

use anyhow::{Result, bail};
use smol_str::SmolStr;
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
    MinusEqual,
    PLUS,
    PlusEqual,
    SEMICOLON,
    SLASH,
    SlashEqual,
    STAR,
    StarEqual,
    QUESTION,
    COLON,

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
    IDENTIFIER(SmolStr),
    STRING(String),
    NUMBER(f64),

    AND,
    CLASS,
    ELSE,
    FALSE,
    TRUE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    VAR,
    WHILE,

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
    Variable(SmolStr),
    Assign(SmolStr, Box<Expr>),
    Logic(Box<LogicExpr>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct LogicExpr {
    pub is_and: bool,
    pub left: Expr,
    pub right: Expr
}
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Boolean(bool),
    Nil,
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

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Num(f64),
    Nil,
    String(String),
    // Object(Object),
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Num(num) => write!(f, "{}",  num),
            Value::Nil => write!(f, "nil"),
            Value::String(content) => write!(f, "{}", content),
        }
    }
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

#[derive(Debug, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    VarDecl(SmolStr, Option<Expr>),
    BlockStmt(Vec<Stmt>),
    IF(Box<IfStmt>),
    While(Box<WhileStmt>),
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Stmt,
    pub els: Option<Stmt>
}
#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Stmt,
}